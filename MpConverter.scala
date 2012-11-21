package com.entwinemedia.mpconverter

import java.io.{FileInputStream, FileOutputStream, File}
import java.util.UUID
import scala.xml._
import org.apache.commons.io.FilenameUtils
import org.opencastproject.util.{MimeTypes, MimeType, ZipUtil}
import com.entwinemedia.util._
import com.entwinemedia.util.Extractors.Number
import com.entwinemedia.util.Terminal._
import org.opencastproject.mediapackage._
import MediaPackageElements.MANIFEST_FILENAME
import annotation.tailrec
import java.net.URI
import math._
import scala.Left
import scala.Some
import scala.Right

/**
 * Convert a zipped 1.3 media package into a 1.4 one or create it from a zipped directory structure.
 */
object MpConverter {
  import EitherImplicits._
  import Pipe._
  import Trial._
  import Console.{readMimeType, readFlavor, readMpeType}

  def main(args: Array[String]) {
    println("""MediaPackage Converter
              |Fixes media package XML namespace issue or builds a manifest from a directory structure.
              |Use tab completion when editing element infos.
            """.stripMargin)
    val result = for {
      name <- args.headOption.toRight("please provide a zip filename")
      doz <- getZipOrDir(name)
    } yield {
      if (doz.isFile) {
        println("Unzipping media package...")
        Io.withTmpDir(unzip(doz))(convert _).tryMsg
      }
      else
        convert(doz).tryMsg
    }
    result.fold(msg => println(style(Red)(msg)), println _)
  }

  val ZIP_EXT = ".zip"

  def getZipOrDir(name: String): Either[String, File] = {
    val doz = new File(name)
    if (doz.isFile) {
      if (name.takeRight(ZIP_EXT.length).equalsIgnoreCase(ZIP_EXT))
        Right(doz)
      else
        Left(name + " is not a zip file")
    } else if (doz.isDirectory) {
      Right(doz)
    } else {
      Left(name + " does not exist")
    }
  }

  /** Convert a MH 1.3 media package or a media package without manifest into a MH 1.4 compliant one. */
  def convert(root: File): File = {
    val (manifest, mp) = getManifest(root) match {
      case Some(manifest) =>
        println("Found manifest. Fixing namespace.")
        (manifest, fixNamespace(manifest))
      case None =>
        val mp = buildMediaPackage(root)
        (saveMediaPackage(mp, root), mp)
    }
    zipMp(BFile(manifest, root), mp)
  }

  /** Unzip the given file to a temporary directory which is returned. */
  def unzip(zip: File): File = TmpDir.dirFor(zip) &> (ZipUtil.unzip(zip, _: File))

  val RelativeFileUrl = "file://([^/].+)".r
  val RelativePath = "([^/].+)".r

  /** Zip a media package.
    * @return the zipped media package. */
  def zipMp(manifest: BFile, mp: MediaPackage): File = {
    def mkFile(uri: URI): Option[BFile] = uri.toString match {
      case RelativeFileUrl(path) => Some(BFile(manifest.base, path))
      case RelativePath(path) => Some(BFile(manifest.base, path))
      case _ => None
    }
    println("Zipping media package...")
    val files = manifest :: mp.getElements.toList.flatMap(e => mkFile(e.getURI))
    for (file <- files) println(" " + file.relativePath)
    TmpDir.zipFor(manifest.base) &> (_.delete()) &> (Zip.zip(files, _: File))
  }

  /** Creates a tmp dir for a file and a zip file name from a tmp dir. */
  object TmpDir {
    val uuidLength = UUID.randomUUID().toString.length
    def dirFor(zip: File) = new File(zip.getParentFile, FilenameUtils.getBaseName(zip.getName) + "-" + UUID.randomUUID().toString)
    def zipFor(dir: File) = {
      val zipName = dir.getName.dropRight(uuidLength) match {
        case "" => dir.getName + "-1.4.zip"
        case n => n + "1.4.zip"
      }
      new File(dir.getParentFile, zipName)
    }
  }

  /** Complete mpe by guessing mime type, flavor and type. */
  def guess(mpe: Mpe): Mpe = {
    import MediaPackageElements._
    import MimeTypes._
    import MediaPackageElement.Type._
    val mimeType = FilenameUtils.getExtension(mpe.name).toLowerCase match {
      case "mp4" => Some(MPEG4)
      case "xml" => Some(XML)
      case "txt" => Some(TEXT)
      case "md5" => Some(TEXT)
      case "pdf" => Some(MimeTypes.fromString("application/pdf"))
      case "mov" => Some(MimeTypes.fromString("video/quicktime"))
      case "wav" => Some(MimeTypes.fromString("audio/x-wav"))
      case _ => None
    }
    val flavor = (mpe.name.toLowerCase, mimeType) match {
      case (_, Some(TEXT)) => None
      case (name, Some(XML)) if name.startsWith("mpeg-7") => Some(TEXTS)
      case (name, _) if name.startsWith("camera") => Some(PRESENTER_SOURCE)
      case (name, _) if name.startsWith("screen") => Some(PRESENTATION_SOURCE)
      case (name, Some(XML)) if name.startsWith("episode") => Some(EPISODE)
      case (name, Some(XML)) if name.startsWith("series") => Some(SERIES)
      case _ => None
    }
    val mpeType = mimeType.map {
      case MPEG4 => Track
      case XML => Catalog
      case _ => Attachment
    }
    mpe.copy(mimeType = mimeType, flavor = flavor, mpeType = mpeType)
  }

  /** Get media package manifest from a media package root dir. */
  def getManifest(root: File): Option[File] = root.listFiles.toList.find(_.getName.toLowerCase match {
    case "mediapackage.xml" | "index.xml" | "manifest.xml" => true
    case _ => false
  })

  def fixNamespace(manifest: File): MediaPackage = {
    import XmlXform._
    val mp = XML.loadFile(manifest)
    val prefix = (mp \\ "mediapackage").head.scope.prefix
    val fixedMp = xform {
      case e: Elem if e.prefix == null => e.copy(prefix = prefix)
    }(mp)
    saveManifest(manifest, fixedMp)
    Io.use(new FileInputStream(manifest))(in => mpBuilder.loadFromXml(in))
  }

  /** Create a new manifest in dialog with the user. */
  def buildMediaPackage(root: File): MediaPackage = {
    val mpes = findMpElems(root).map(guess _)
    completeMpes(mpes, showTable = true).map(mediaPackageElementFrom _) |> (newMediaPackage _)
  }

  val mpBuilder = MediaPackageBuilderFactory.newInstance.newMediaPackageBuilder

  val mpeBuilder = MediaPackageElementBuilderFactory.newInstance.newElementBuilder

  /** Create a new MediaPackage from a list of elements. */
  def newMediaPackage(mpes: List[MediaPackageElement]): MediaPackage = mpBuilder.createNew() &> (mp => mpes.foreach(mp.add _))

  /** Convert an Mpe into a MediaPackageElement. */
  def mediaPackageElementFrom(mpe: Mpe): MediaPackageElement =
    mpeBuilder.elementFromURI(mpe.file.relativePathAsUri, mpe.mpeType.get, mpe.flavor.get)

  sealed trait UserInput
  case object Ok extends UserInput
  case object ForceOk extends UserInput
  case object Continue extends UserInput

  /** Complete media package element information. */
  @tailrec
  def completeMpes(mpes: List[Mpe], showTable: Boolean): List[Mpe] = {
    import Console.console.readLine
    if (showTable) MpeTableOutput.display(mpes)
    val (newMpes, ok) = readLine(style(Bold)("Enter number, 'ok' or 'ok!' > ")) match {
      case Number(nr) if nr < mpes.size =>
        val mpe = mpes(nr)
        println("\nFixing " + mpe.id + " -> " + mpe.name)
        val mpeType = readMpeType(mpe.mpeType)
        val flavor = readFlavor(mpe.flavor)
        val mimeType = readMimeType(mpe.mimeType)
        if (mimeType.isDefined || flavor.isDefined || mpeType.isDefined) {
          val patch = mpe.copy(
            flavor = flavor.orElse(mpe.flavor),
            mimeType = mimeType.orElse(mpe.mimeType),
            mpeType = mpeType.orElse(mpe.mpeType))
          val patched = mpes.patch(nr, patch :: Nil, 1)
          (patched, false)
        } else {
          (mpes, false)
        }
      case "ok" => (mpes, Ok)
      case "ok!" => (mpes, ForceOk)
      case "q" => throw new RuntimeException("quit")
      case _ =>
        println(style(Yellow)("Type a number, 'ok' when finished, 'ok!' to force finishing or 'q' to quit."))
        (mpes, Continue)
    }
    //
    (isComplete(newMpes), ok) match {
      case (true, Ok) => newMpes
      case (_, ForceOk) => newMpes.filter(isComplete _)
      case (false, Ok) =>
        println(style(Red)("Not yet complete. Force media package creation with 'ok!'. This strips all incomplete elements."))
        completeMpes(newMpes, showTable = false)
      case _ => completeMpes(newMpes, showTable = true)
    }
  }

  object MpeTableOutput {
    // padding functions and prefixes
    private val pad = List(("", padr _), ("| ", padl _), (" | ", padl _), (" |", padl _), ("|", padl _), ("|", padl _))

    private def padr(w: Int, a: String) = (" " * max(0, w - a.length)) + a
    private def padl(w: Int, a: String) = a + (" " * max(0, w - a.length))

    private def display[A](a: Option[A]) = a.map(_.toString).getOrElse("<unknown>")

    /** Print the list of media package elements. */
    def display(mpes: List[Mpe]) {
      // serialize to strings
      val rows = for ((mpe, index) <- mpes.zipWithIndex) yield {
        import mpe._
        List(index.toString, id.toString, name.toString, display(mpeType), display(flavor), display(mimeType))
      }
      val rows_ = List("Nr", "Id", "Name", "Type", "Flavor", "MimeType") :: rows
      // calculate the maximum word width in each column
      val wordWidths = rows_.map(_.map(_.length)).transpose.map(_.max)
      def outRow(row: List[String]) = ("" /: (row, wordWidths, pad).zipped) {case (sum, (col, w, (sep, p))) => sum + sep + p(w, col)}
      // print table header
      println(style(Bold)(outRow(rows_.head)))
      // print table body
      for ((row, mpe) <- rows zip mpes) {
        val style_ = if (notComplete(mpe)) ClearStyle else Green
        println(style(style_)(outRow(row)))
      }
    }
  }

  def isComplete(mpes: List[Mpe]): Boolean = !mpes.exists(mpe => notComplete(mpe))

  def notComplete(mpe: Mpe): Boolean = mpe.mimeType.isEmpty || mpe.flavor.isEmpty

  def isComplete(mpe: Mpe) = !notComplete(mpe)

  /** Find all possible media package elements inside a root dir. */
  def findMpElems(root: File): List[Mpe] = for {
    subDir <- root.listFiles.toList.filter(_.isDirectory)
    file <- subDir.listFiles.filter(_.isFile)
  } yield Mpe(BFile(file, root), subDir.getName, file.getName)

  /** Save a manifest. */
  def saveManifest(manifest: File, mp: Node) { XML.save(manifest.getAbsolutePath, mp, enc = "utf-8", xmlDecl = true, doctype = null) }

  /** Save a media package object and return the file. */
  def saveMediaPackage(mp: MediaPackage, dir: File): File =
    (new File(dir, MANIFEST_FILENAME)) &> (f => Io.use(new FileOutputStream(f))(MediaPackageParser.getAsXml(mp, _, true)))

  def zip4[A, B, C, D](as: List[A], bs: List[B], cs: List[C], ds: List[D]): List[(A, B, C, D)] = (as, bs, cs, ds) match {
    case (a :: as, b :: bs, c :: cs, d :: ds) => (a, b, c, d) :: zip4(as, bs, cs, ds)
    case _ => Nil
  }

  @tailrec
  def zip4[A, B, C, D](sum: List[(A, B, C, D)], as: List[A], bs: List[B], cs: List[C], ds: List[D]): List[(A, B, C, D)] =
    (as, bs, cs, ds) match {
      case (a :: as, b :: bs, c :: cs, d :: ds) => zip4((a, b, c, d) :: sum, as, bs, cs, ds)
      case _ => sum.reverse
    }
}

/**
 * A media package element representation.
 */
case class Mpe(file: BFile,
               id: String, name: String,
               mpeType: Option[MediaPackageElement.Type] = None,
               mimeType: Option[MimeType] = None,
               flavor: Option[MediaPackageElementFlavor] = None)

/**
 * Combine user input related stuff.
 */
object Console {
  import MimeTypes._
  import org.opencastproject.mediapackage.MediaPackageElements._
  import MediaPackageElement.Type
  import MediaPackageElement.Type._
  import Pipe._
  import Trial._
  import jline.{Completor, SimpleCompletor, ConsoleReader}

  val mimeTypes = Array(XML, TEXT, JSON, JPG, MJPEG, MPEG4, MPEG4_AAC, DV, MJPEG2000, MP3, AAC, CALENDAR, ZIP, JAR)
  val flavors = Array(MEDIAPACKAGE_COVER_FLAVOR, PRESENTER_SOURCE, PRESENTATION_SOURCE, AUDIENCE_SOURCE, DOCUMENTS_SOURCE,
    INDEFINITE_SOURCE, EPISODE, SERIES, SEGMENTS, TEXTS, SPEECH, CHAPTERING, PRESENTER_PLAYER_PREVIEW,
    PRESENTATION_PLAYER_PREVIEW, PRESENTER_SEARCHRESULT_PREVIEW, PRESENTATION_SEARCHRESULT_PREVIEW,
    PRESENTER_SEGMENT_PREVIEW, PRESENTATION_SEGMENT_PREVIEW, PRESENTER_FEED_PREVIEW, PRESENTATION_FEED_PREVIEW,
    XACML_POLICY_EPISODE, XACML_POLICY_SERIES, XACML_POLICY, CAPTION_GENERAL, CAPTION_DFXP_FLAVOR, YOUTUBE)
  val mpeTypes = Array(Manifest, Timeline, Track, Catalog, Attachment, Other)

  // consoles to read input from user
  val mimeTypeConsole = newConsole(new SimpleCompletor(mimeTypes.map(_.toString)))
  val flavorConsole = newConsole(new SimpleCompletor(flavors.map(_.toString)))
  val mpeTypeConsole = newConsole(new SimpleCompletor(mpeTypes.map(_.toString)))
  val console = new ConsoleReader()

  private def newConsole(completor: Completor) = (new ConsoleReader) &> {
    c =>
      c.addCompletor(completor)
      c.setUseHistory(true)
  }

  def readMimeType(current: Option[MimeType]) = read(mimeTypeConsole, "MimeType", current)(MimeTypes.parseMimeType _)
  def readFlavor(current: Option[MediaPackageElementFlavor]) = read(flavorConsole, "Flavor", current)(MediaPackageElementFlavor.parseFlavor _)
  def readMpeType(current: Option[MediaPackageElement.Type]) = read(mpeTypeConsole, "Type", current)(Type.valueOf _)

  @tailrec
  def read[A](console: ConsoleReader, msg: String, default: Option[A])(converter: String => A): Option[A] = {
    console.readLine(style(Bold)(msg + " [" + default.getOrElse("") + "] > ")).trim match {
      case "" => None
      case a => converter(a).tryOpt match {
        case a@Some(_) => a
        case _ =>
          println(style(Red)("Invalid input. Try tab completion."))
          read(console, msg, default)(converter)
      }
    }
  }
}