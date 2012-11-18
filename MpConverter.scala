package com.entwinemedia.mpconverter

import java.io.File
import java.util.UUID
import scala.xml._
import org.apache.commons.io.FilenameUtils
import org.opencastproject.util.{MimeTypes, MimeType, FileSupport, ZipUtil}
import com.entwinemedia.util._
import org.opencastproject.mediapackage.{MediaPackage, MediaPackageElementFlavor}

object MpConverter {
  import EitherImplicits._
  import Pipe._
  import Trial._

  def main(args: Array[String]) {
    val result = for {
      zipName <- args.headOption.toRight("please provide a zip filename")
      zipFile <- getZipFile(zipName)
    } yield convert(zipFile)
    output(result)
  }

  def output[A](result: Either[String, A]) {
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

  def buildManifest(root: File) = {
    val mpes = for {
      mpe <- findMpElems(root)
      (mimeType, flavor) = guessMimeTypeAndFlavor(mpe.file)
    } yield mpe.copy(mimeType = mimeType, flavor = flavor)

    def opt(s: String) = if (s != "") Some(s) else None

    def confirm(mpes: List[Mpe]): List[Mpe] = {
      displayMpes(mpes)
      print("[nr|ok]> ")
      val (newMpes, ok) = readLine() match {
        case Number(nr) if nr < mpes.size =>
          val mpe = mpes(nr)
          val mimeType = readLine("MimeType [" + mpe.mimeType.getOrElse("") + "] ")
          val flavor = readLine("Flavor [" + mpe.flavor.getOrElse("") + "] ")
          if (mimeType != "" || flavor != "") {
            val patched = mpes.patch(nr, List(mpe.copy(
              flavor = if (flavor != "") Some(MediaPackageElementFlavor.parseFlavor(flavor)) else mpe.flavor,
              mimeType = if (mimeType != "") Some(MimeTypes.parseMimeType(mimeType)) else mpe.mimeType)
            ), 1)
            (patched, false)
          } else {
            (mpes, false)
          }
        case "ok" => (mpes, true)
        case _ => (mpes, false)
      }
      if (notComplete(newMpes) || !ok) confirm(newMpes) else newMpes
    }
    confirm(mpes)
  }

  object Number {
    val Regex = "([0-9]+)".r
    def unapply(s: String): Option[Int] = s match {
      case Regex(nr) => Some(nr.toInt)
      case _ => None
    }
  }

  def displayMpes(mpes: List[Mpe]) {
    def display[A](a: Option[A]) = a.getOrElse("<unknown>")
    for ((mpe, index) <- mpes.zipWithIndex)
      println(index + ": " + mpe.id + ", " + mpe.name + ": " + display(mpe.mimeType) + ", " + display(mpe.flavor))
  }

  def notComplete(mpes: List[Mpe]): Boolean = mpes.exists(mpe => notComplete(mpe))

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