package com.entwinemedia.util

import _root_.jline.ANSIBuffer.ANSICodes._
import _root_.jline.{Terminal => JTerminal}
import xml.{NodeSeq, Node}
import xml.transform.{RewriteRule, RuleTransformer}
import scala.Left
import scala.Right
import scala.Some
import java.io.File
import org.opencastproject.util.FileSupport

object XmlXform {
  def xform(f: PartialFunction[Node, NodeSeq]): RuleTransformer = new RuleTransformer(rule(f))

  def rule(f: PartialFunction[Node, NodeSeq]): RewriteRule = new RewriteRule {
    override def transform(n: Node) = if (f.isDefinedAt(n)) f(n) else n
  }
}

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

object Trial {
  import EitherImplicits._

  class Trial[A](f: => A) {
    def tryMsg = try {f.success} catch {case e: Exception => e.getMessage.fail}

    def tryOpt = try {Some(f)} catch {case e: Exception => None}
  }

  implicit def _Any_Trial[A](f: => A): Trial[A] = new Trial(f)
}

object Extractors {
  /** Number extractor. */
  object Number {
    val Regex = "([0-9]+)".r

    def unapply(s: String): Option[Int] = s match {
      case Regex(nr) => Some(nr.toInt)
      case _ => None
    }
  }
}

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

/**
 * Terminal utilities. Based on jLine.
 */
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
