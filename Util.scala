package com.entwinemedia.util

import jline.ANSIBuffer.ANSICodes._
import jline.{Terminal => JTerminal}
import scala.Left
import scala.Right
import scala.Some
import xml.{NodeSeq, Node}
import xml.transform.{RewriteRule, RuleTransformer}
import java.io.File
import java.net.{URLEncoder, URI}
import org.opencastproject.util.FileSupport

/** XML transformations. */
object XmlXform {
  def xform(f: PartialFunction[Node, NodeSeq]): RuleTransformer = new RuleTransformer(rule(f))

  def rule(f: PartialFunction[Node, NodeSeq]): RewriteRule = new RewriteRule {
    override def transform(n: Node) = if (f.isDefinedAt(n)) f(n) else n
  }
}

/** Implicits for Either. */
object EitherImplicits {
  final class ToLeft[A](a: A) {
    def fail = Left(a)
  }

  final class ToRight[B](b: B) {
    def success = Right(b)
  }

  implicit def _Any_Left[A](a: A): ToLeft[A] = new ToLeft(a)
  implicit def _Any_Right[B](b: B): ToRight[B] = new ToRight(b)

  /** This implicit conversion allows an Either to be used in a for comprehension. */
  implicit def _Either_RightProjection[A, B](a: Either[A, B]): Either.RightProjection[A, B] = a.right

  def test() {
    val s: Either[String, Int] = 1.success
    val f: Either[String, Int] = "hello".fail
    println(f)
    println(s)
    val r = for {
      v1 <- s
      v2 <- s
      v3 <- s
    } yield v1 + v2 + v3
    println("r=" + r)
  }
}

/** Passing values. */
object Pipe {
  class Pipe[A](a: A) {
    def |>[B](f: A => B): B = f(a)
  }

  class SideEffect[A](a: A) {
    def &>(f: A => Unit): A = {f(a); a}
  }

  implicit def _Any_Pipe[A](a: A): Pipe[A] = new Pipe(a)
  implicit def _Any_SideEffect[A](a: A): SideEffect[A] = new SideEffect(a)
}

/** Functional try catch. */
object Trial {
  import EitherImplicits._

  class Trial[A](f: => A) {
    def tryMsg = try {f.success} catch {case e: Exception => e.getMessage.fail}

    def tryEx = try {f.success} catch {case e: Exception => e.fail}

    def tryOpt = try {Some(f)} catch {case e: Exception => None}
  }

  implicit def _Any_Trial[A](f: => A): Trial[A] = new Trial(f)
}

/** All purpose extractors. */
object Extractors {
  /** Number extractor. */
  object Number {
    val Regex = "([0-9]+)".r

    def unapply(s: String): Option[Int] = s match {
      case Regex(nr) => Some(nr.toInt)
      case _ => None
    }
  }

  /**
   * Extracts file information.
   * @param combinedExtensions true: file.tar.gz => tar.gz, false: gz
   */
  class FileInfo(combinedExtensions: Boolean) {
    import com.entwinemedia.util.Extractors.FileInfo.{IsDir, IsFile, NonExistent, FileType}
    import org.apache.commons.io.FilenameUtils._

    def unapply(f: File): Option[(List[String], String, String, FileType)] = {
      val path = f.getAbsolutePath
      val t = if (f.exists) {if (f.isFile) IsFile else IsDir} else NonExistent
      def skip(s: String): String = s match {
        case '.' +++ xs => xs
        case x +++ xs => skip(xs)
        case _ => ""
      }
      val ex = if (combinedExtensions) skip(getName(path)) else skip(getName(path).reverse).reverse
      Some((getPathNoEndSeparator(path).split(File.separator).toList, getBaseName(path), ex, t))
    }
  }

  object FileInfo {
    sealed trait FileType
    case object IsFile extends FileType
    case object IsDir extends FileType
    case object NonExistent extends FileType
  }

  object +++ {
    def unapply(s: String): Option[(Char, String)] = s.headOption.map(h => (h, s.drop(1)))
  }
}

/** IO related functions. */
object Io {
  /** Use a resource ensuring it gets closed afterwards. */
  def use[A <: {def close()}, B](resource: A)(f: A => B) = try {
    f(resource)
  } finally {
    resource.close()
  }

  /** Apply directory `d` to function `f` deleting the directory afterwards. */
  def withTmpDir[A](d: File)(f: File => A) = try {
    f(d)
  } finally {
    FileSupport.delete(d, true)
  }
}

/** File with a base dir. `base` must be a directory and a parent of `sub`. */
case class BasedFile(base: File, sub: File) {
  if (!base.isDirectory) throw new Error(base + " is not a directory")
  if (!sub.getAbsolutePath.startsWith(base.getAbsolutePath)) throw new Error(base + " is not a parent of " + sub)

  /** The relative path of `file` related to `base`. */
  lazy val relativePath = sub.getAbsolutePath.drop(base.getAbsolutePath.length + 1)

  lazy val relativePathAsUri = new URI(relativePath.split(File.separator).map(URLEncoder.encode(_, "UTF-8")).mkString("/"))
}

object BasedFile {
  def apply(base: File, path: String): BasedFile = BasedFile(base, new File(base, path))
}

/** Zip */
object Zip {
  // TrueZip
  import de.schlichtherle.io.{File => TFile}

  def zip(files: List[BasedFile], zip: File): File = {
    val tzip = new TFile(zip)
    if (!tzip.isArchive) throw new Error(zip + " is not a zip file")
    for (file <- files) TFile.cp(file, new TFile(tzip, file.relativePath))
    TFile.umount(tzip)
    tzip
  }

  implicit def _File_TFile(f: File): TFile = new TFile(f)
  implicit def _BFile_TFile(f: BasedFile): TFile = new TFile(f.sub)
}

/** Implicits for functions. */
object FunctionImplicits {
  class Composition[B, C](f: B => C) {
    def °[A](g: A => B) = f compose g
  }
  implicit def _Function_Composition[A, B, C](f: B => C) = new Composition(f)
}

/** Terminal utilities. Based on jLine. */
object Terminal {
  val jterminal = JTerminal.getTerminal

  val ClearStyle = attrib(0)

  val Bold = attrib(1)
  val Underscore = attrib(4)
  val Blink = attrib(5)
  val Reverse = attrib(7)
  val Concealed = attrib(8)

  val Black = attrib(30)
  val Red = attrib(31)
  val Green = attrib(32)
  val Yellow = attrib(33)
  val Blue = attrib(34)
  val Magenta = attrib(35)
  val Cyan = attrib(36)
  val White = attrib(37)

  val BgBlack = attrib(40)
  val BgRed = attrib(41)
  val BgGreen = attrib(42)
  val BgYellow = attrib(43)
  val BgBlue = attrib(44)
  val BgMagenta = attrib(45)
  val BgCyan = attrib(46)
  val BgWhite = attrib(47)

  /**
   * Output to the console with a certain setting defined by `attr`.
   * @param attr e.g. `Red` or `Red + Bold`
   */
  def terminal[A](attr: String)(f: => A): A = {
    print(ClearStyle + attr)
    val r = f
    print(ClearStyle)
    r
  }

  /** Create a styled string ready for console output. */
  def style(attribs: String*)(s: String) = ClearStyle + attribs.mkString + s + ClearStyle

  /** Get the current terminal width. */
  def terminalWidth = jterminal.getTerminalWidth

  /** Get the current terminal height. */
  def terminalHeight = jterminal.getTerminalHeight
}
