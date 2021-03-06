package elm


package object language {
  trait Syntax[R[_]] {
    type Sugar[T]
  }

  trait OOSyntax[R[_]] extends Syntax[R] {
    type Sugar[T] = OOSugar[R, T]
  }

  trait OOSugar[R[_], T] { //TODO: should we have variance?
    def lift1[U](f: T => U): R[U]
    def map[U](f: T => U): R[U] = lift1(f)
    def lift2[U,V](s: R[U])(f: (T,U) => V): R[V]
    def zip[U](s: R[U]): R[(T,U)] = lift2(s)((_,_))
    def foldp[U](v: U)(f: (U,T) => U): R[U]
    def async: R[T]
    def +=(f: T => Unit): Unit
  }

  trait ElmPredef[R[_]] {
    implicit def lift0[A](v: A): R[A]
    def timer(hz: Int): R[Int]
    def mouse: R[(Int, Int)]
    def key: R[Option[Int]]
  }

  trait ElmLang[R[_], S <: Syntax[R]] extends Syntax[R] { self: ElmPredef[R] =>
    implicit def enrich[T](r: R[T]): S#Sugar[T]
  }


  trait ElmOO[R[_]] extends ElmLang[R, OOSyntax[R]] {
    self: ElmPredef[R] =>
  }
}
