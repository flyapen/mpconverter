package com.entwinemedia.util

import xml.{NodeSeq, Node}
import xml.transform.{RewriteRule, RuleTransformer}

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