package elm.semantics.actors

import akka.actor.Props
import elm.language._
import elm.domain.Actors.{Dispatcher, Factory, Reactive}

import scala.language.implicitConversions

object Language extends ElmOO[Reactive] with ElmPredef[Reactive] {
  implicit def lift0[A](v: A): Reactive[A] = Factory.mkLift0(v)
  def timer(hz: Int): Reactive[Int] = ??? //TODO
  def mouse: Reactive[(Int, Int)] = Factory.mouse
  def key: Reactive[Option[Int]] = Factory.key

  implicit def enrich[T](r: Reactive[T]): OOSyntax[Reactive]#Sugar[T] = new OOSugar[Reactive, T] {
    def lift1[U](f: T => U) = Factory.mkLift1(r, f)

    def lift2[U, V](s: Reactive[U])(f: (T, U) => V) = Factory.mkLift2(r, s)(f)

    def foldp[U](v: U)(f: (U, T) => U) = Factory.mkFoldp(r)(v)(f)

    def async = Factory.mkAsync(r)

    def +=(f: (T) => Unit) = Factory.attach(r, f)
  }
}
