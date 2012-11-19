package com.entwinemedia.mpconverter

import java.io.File
import java.util.UUID
import scala.xml._
import org.apache.commons.io.FilenameUtils
import org.opencastproject.util.{MimeTypes, MimeType, FileSupport, ZipUtil}
import com.entwinemedia.util._
import com.entwinemedia.util.Extractors.Number
import com.entwinemedia.util.Terminal._
import org.opencastproject.mediapackage.{MediaPackage, MediaPackageElementFlavor}
import scala.Left
import scala.Right
import scala.Some

object MpConverter {
  import EitherImplicits._
  import Pipe._
  import Trial._
  import Console.{readMimeType, readFlavor}

  def main(args: Array[String]) {
    val result = for {
      zipName <- args.headOption.toRight("please provide a zip filename")
      zipFile <- getZipFile(zipName)
    } yield convert(zipFile)
    result.fold(msg => println("[ERROR] " + msg), println _)
  }

  def getZipFile(name: String): Either[String, File] = {
    val zip = new File(name)
    if (zip.isFile && name.takeRight(4).equalsIgnoreCase(".zip"))
      Right(zip)
    else
      Left(name + " does not exist or is probably not a zip file")
  }

  /** Convert a MH 1.3 media package or a media package without manifest into a MH 1.4 compliant one. */
  def convert(zipFile: File): Either[String, Any] = withTmpDir(unzip(zipFile)) {
    dir =>
      (getManifest(dir) match {
        case Some(manifest) => fixNamespace(manifest)
        case None => buildManifest(dir)
      }) &> (println _)
      zip(dir)
  }.tryMsg

  /** Unzip the given file to a temporary directory which is returned. */
  def unzip(zip: File): File = TmpDir.toDir(zip) &> (ZipUtil.unzip(zip, _: File))

  def zip(dir: File): File = TmpDir.toZip(dir) &> (_.delete()) &> (ZipUtil.zip(Array(dir), _: File, true))

  /** Creates a tmp dir for a file and a file name (for the zip) for a tmp dir. */
  object TmpDir {
    def toDir(zip: File) = new File(zip.getParentFile, FilenameUtils.getBaseName(zip.getName) + "-" + UUID.randomUUID().toString)
    def toZip(dir: File) = new File(dir.getParentFile, dir.getName.dropRight(UUID.randomUUID().toString.length) + "1.4.zip")
  }

  def guessMimeTypeAndFlavor(mpe: File): (Option[MimeType], Option[MediaPackageElementFlavor]) = {
    val mimeType = FilenameUtils.getExtension(mpe.getName) match {
      case "" => None
      case ext => MimeTypes.fromSuffix(ext).tryOpt
    }
    (mimeType, None)
  }

  /** Apply directory d to function zip deleting d afterwards. */
  def withTmpDir[A](d: File)(f: File => A) = try {
    f(d)
  } finally {
    FileSupport.delete(d, true)
  }

  def getManifest(root: File): Option[File] = root.listFiles.toList.find(_.getName == "mediapackage.xml")

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
  def buildManifest(root: File) = {
    import Console.console.readLine
    val mpes = for {
      mpe <- findMpElems(root)
      (mimeType, flavor) = guessMimeTypeAndFlavor(mpe.file)
    } yield mpe.copy(mimeType = mimeType, flavor = flavor)

    def confirm(mpes: List[Mpe]): List[Mpe] = {
      displayMpes(mpes)
      terminal(Bold)(print("Enter number or 'ok' > "))
      val (newMpes, ok) = readLine() match {
        case Number(nr) if nr < mpes.size =>
          val mpe = mpes(nr)
          val mimeType = readMimeType(mpe.mimeType)
          val flavor = readFlavor(mpe.flavor)
          if (mimeType.isDefined || flavor.isDefined) {
            val patch = mpe.copy(
              flavor = flavor.map(a => Some(MediaPackageElementFlavor.parseFlavor(a))).getOrElse(mpe.flavor),
              mimeType = mimeType.map(a => Some(MimeTypes.parseMimeType(a))).getOrElse(mpe.mimeType)) :: Nil
            val patched = mpes.patch(nr, patch, 1)
            (patched, false)
          } else {
            (mpes, false)
          }
        case "ok" => (mpes, true)
        case "q" => throw new RuntimeException("quit")
        case _ => (mpes, false)
      }
      (isComplete(newMpes), ok) match {
        case (true, true) => newMpes
        case (false, true) => terminal(Red)(println("Not yet complete")); confirm(newMpes)
        case _ => confirm(newMpes)
      }
    }
    confirm(mpes)
  }

  /** Print the list of media package elements. */
  def displayMpes(mpes: List[Mpe]) {
    def display[A](a: Option[A]) = a.getOrElse("<unknown>")
    for ((mpe, index) <- mpes.zipWithIndex)
      println(index + ": " + mpe.id + ", " + mpe.name + ": " + display(mpe.mimeType) + ", " + display(mpe.flavor))
  }

  def isComplete(mpes: List[Mpe]): Boolean = !mpes.exists(mpe => notComplete(mpe))

  def notComplete(mpe: Mpe): Boolean = mpe.mimeType.isEmpty || mpe.flavor.isEmpty

  case class Mpe(file: File, id: String, name: String,
                 mimeType: Option[MimeType] = None, flavor: Option[MediaPackageElementFlavor] = None)

  /** Find all possible media package elements inside a root dir. */
  def findMpElems(root: File): List[Mpe] = for {
    subDir <- root.listFiles.toList.filter(_.isDirectory)
    file <- subDir.listFiles.filter(_.isFile)
  } yield Mpe(file, subDir.getName, file.getName)

  /** Save a manifest. */
  def saveManifest(manifest: File, mp: Node) { XML.save(manifest.getAbsolutePath, mp, enc = "utf-8", xmlDecl = true, doctype = null) }
}

/** Combine user input related stuff. */
object Console {
  import MimeTypes._
  import org.opencastproject.mediapackage.MediaPackageElements._
  import Pipe._
  import jline.{Completor, SimpleCompletor, ConsoleReader}

  val mimeTypes = Array(XML, TEXT, JSON, JPG, MJPEG, MPEG4, MPEG4_AAC, DV, MJPEG2000, MP3, AAC, CALENDAR, ZIP, JAR)
  val flavors = Array(MEDIAPACKAGE_COVER_FLAVOR, PRESENTER_SOURCE, PRESENTATION_SOURCE, AUDIENCE_SOURCE, DOCUMENTS_SOURCE, INDEFINITE_SOURCE, EPISODE, SERIES, SEGMENTS, TEXTS, SPEECH, CHAPTERING, PRESENTER_PLAYER_PREVIEW, PRESENTATION_PLAYER_PREVIEW, PRESENTER_SEARCHRESULT_PREVIEW, PRESENTATION_SEARCHRESULT_PREVIEW, PRESENTER_SEGMENT_PREVIEW, PRESENTATION_SEGMENT_PREVIEW, PRESENTER_FEED_PREVIEW, PRESENTATION_FEED_PREVIEW, XACML_POLICY_EPISODE, XACML_POLICY_SERIES, XACML_POLICY, CAPTION_GENERAL, CAPTION_DFXP_FLAVOR, YOUTUBE)
  
  val mimeTypeCompletor = new SimpleCompletor(mimeTypes.map(_.toString))
  val flavorCompletor = new SimpleCompletor(flavors.map(_.toString))

  /** Console to read input from user. */
  val mimeTypeConsole = newConsole(mimeTypeCompletor)
  val flavorConsole = newConsole(flavorCompletor)
  val console = new ConsoleReader()

  private def newConsole(completor: Completor) = (new ConsoleReader) &> {
    c =>
      c.addCompletor(mimeTypeCompletor)
      c.setUseHistory(true)
  }

  def readMimeType(current: Option[MimeType]) = read(mimeTypeConsole, "MimeType", current)
  def readFlavor(current: Option[MediaPackageElementFlavor]) = read(mimeTypeConsole, "MimeType", current)

  def read[A](console: ConsoleReader, msg: String, default: Option[A]): Option[String] = {
    console.readLine(style(Bold)(msg + " [" + default.getOrElse("") + "] > ")).trim |> (input => if (input != "") Some(input) else None)
  }
}