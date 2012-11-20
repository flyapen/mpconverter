package com.entwinemedia.mpconverter

import java.io.{FileOutputStream, File}
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
import java.net.{URLEncoder, URI}
import math._
import scala.Left
import scala.Some
import scala.Right

/** Convert a zipped 1.3 media package into a 1.4 one or create it from a zipped directory structure. */
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
      zipName <- args.headOption.toRight("please provide a zip filename")
      zipFile <- getZipFile(zipName)
    } yield convert(zipFile)
    result.fold(msg => println(style(Red)(msg)), println _)
  }

  val ZIP_EXT = ".zip"

  def getZipFile(name: String): Either[String, File] = {
    val zip = new File(name)
    if (zip.isFile && name.takeRight(ZIP_EXT.length).equalsIgnoreCase(ZIP_EXT))
      Right(zip)
    else
      Left(name + " does not exist or is probably not a zip file")
  }

  /** Convert a MH 1.3 media package or a media package without manifest into a MH 1.4 compliant one. */
  def convert(zipFile: File): Either[String, Any] = Io.withTmpDir(unzip(zipFile)) {
    dir =>
      getManifest(dir) match {
        case Some(manifest) => fixNamespace(manifest)
        case None =>
          val mp = buildMediaPackage(dir)
          saveMediaPackage(mp, dir)
      }
      zipContent(dir)
  }.tryMsg

  /** Unzip the given file to a temporary directory which is returned. */
  def unzip(zip: File): File = TmpDir.toDir(zip) &> (ZipUtil.unzip(zip, _: File))

  /** Zip the _content_ of a directory. */
  def zipContent(dir: File): File = TmpDir.toZip(dir) &> (_.delete()) &> (ZipUtil.zip(dir.listFiles, _: File, true))

  /** Creates a tmp dir for a file and a file name (for the zip) for a tmp dir. */
  object TmpDir {
    val uuidLength = UUID.randomUUID().toString.length
    def toDir(zip: File) = new File(zip.getParentFile, FilenameUtils.getBaseName(zip.getName) + "-" + UUID.randomUUID().toString)
    def toZip(dir: File) = new File(dir.getParentFile, dir.getName.dropRight(uuidLength) + "1.4.zip")
  }

  /** Complete mpe by guessing mime type, flavor and type. */
  def complete(mpe: Mpe): Mpe = {
    import MediaPackageElements._
    import MimeTypes._
    import MediaPackageElement.Type._
    val mimeType = FilenameUtils.getExtension(mpe.name).toLowerCase match {
      case "mp4" => Some(MPEG4)
      case "xml" => Some(XML)
      case "txt" => Some(TEXT)
      case "pdf" => Some(MimeTypes.fromString("application/pdf"))
      case "mov" => Some(MimeTypes.fromString("video/quicktime"))
      case "wav" => Some(MimeTypes.fromString("audio/x-wav"))
      case _ => None
    }
    val flavor = (mpe.name.toLowerCase, mimeType) match {
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
  def getManifest(root: File): Option[File] = root.listFiles.toList.find(f => f.getName == "mediapackage.xml" || f.getName == "index.xml")

  def fixNamespace(manifest: File) {
    import XmlXform._
    val mp = XML.loadFile(manifest)
    val prefix = (mp \\ "mediapackage").head.scope.prefix
    val fixedMp = xform {
      case e: Elem if e.prefix == null => e.copy(prefix = prefix)
    }(mp)
    saveManifest(manifest, fixedMp)
  }

  /** Create a new manifest in dialog with the user. */
  def buildMediaPackage(root: File): MediaPackage = {
    val mpes = findMpElems(root).map(complete _)
    completeMpes(mpes, true).map(mediaPackageElementFrom _) |> (newMediaPackage _)
  }

  val mpBuilder = MediaPackageBuilderFactory.newInstance.newMediaPackageBuilder

  val mpeBuilder = MediaPackageElementBuilderFactory.newInstance.newElementBuilder

  def newMediaPackage(mpes: List[MediaPackageElement]): MediaPackage = mpBuilder.createNew() &> (mp => mpes.foreach(mp.add _))

  def mediaPackageElementFrom(mpe: Mpe): MediaPackageElement = {
    val path = mpe.file.getPath.replaceAll(File.separator, "/").split("/").map(URLEncoder.encode(_, "UTF-8")).mkString("/")
    mpeBuilder.elementFromURI(new URI("file://" + path), mpe.mpeType.get, mpe.flavor.get)
  }

  /** Complete media package element information. */
  @tailrec
  def completeMpes(mpes: List[Mpe], showTable: Boolean): List[Mpe] = {
    import Console.console.readLine
    if (showTable) MpeTableOutput.display(mpes)
    val (newMpes, ok) = readLine(style(Bold)("Enter number or 'ok' > ")) match {
      case Number(nr) if nr < mpes.size =>
        val mpe = mpes(nr)
        println("\n" + mpe.id + " -> " + mpe.name)
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
      case "ok" => (mpes, true)
      case "q" => throw new RuntimeException("quit")
      case _ =>
        println(style(Yellow)("Type a number, 'ok' when finished or 'q' to quit."))
        (mpes, false)
    }
    //
    (isComplete(newMpes), ok) match {
      case (true, true) => newMpes
      case (false, true) =>
        println(style(Red)("Not yet complete. Force media package creation with 'ok!'. This strips all incomplete elements."))
        completeMpes(newMpes, false)
      case _ => completeMpes(newMpes, true)
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

  /** Find all possible media package elements inside a root dir. */
  def findMpElems(root: File): List[Mpe] = for {
    subDir <- root.listFiles.toList.filter(_.isDirectory)
    file <- subDir.listFiles.filter(_.isFile)
  } yield Mpe(file, subDir.getName, file.getName)

  /** Save a manifest. */
  def saveManifest(manifest: File, mp: Node) { XML.save(manifest.getAbsolutePath, mp, enc = "utf-8", xmlDecl = true, doctype = null) }

  /** Save a media package object. */
  def saveMediaPackage(mp: MediaPackage, dir: File) {
    Io.use(new FileOutputStream(new File(dir, MANIFEST_FILENAME)))(MediaPackageParser.getAsXml(mp, _, true))
  }

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

case class Mpe(file: File,
               id: String, name: String,
               mpeType: Option[MediaPackageElement.Type] = None,
               mimeType: Option[MimeType] = None,
               flavor: Option[MediaPackageElementFlavor] = None)

/** Combine user input related stuff. */
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