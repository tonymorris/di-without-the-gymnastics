case class ReaderWriterStateT[R, W, S, F[_], A](
  run: (R, S) => F[(W, A, S)]
) {
  def map[B](f: A => B)(implicit F: Functor[F])
  : ReaderWriterStateT[R, W, S, F, B] =
    ReaderWriterStateT {
      case (r, s) => F.map(run(r, s)) {
        case (w, a, s) => (w, f(a), s)
      }
    }

  def flatMap[B](f: A => ReaderWriterStateT[R, W, S, F, B])
                (implicit F: FlatMap[F], W: Semigroup[W])
  : ReaderWriterStateT[R, W, S, F, B] =
    ReaderWriterStateT {
      case (r, s) => F.flatMap(run(r, s)) {
        case (w1, a, s1) => F.map(f(a) run (r, s1)) {
          case (w2, b, s2) => (W.op(w1, w2), b, s2)
        }
      }
    }
}

object ReaderWriterStateT {
  type ReaderWriterState[R, W, S, A] =
  ReaderWriterStateT[R, W, S, Id, A]
}

case class ReaderT[A, F[_], B](
                                  rd: A => F[B]
                                  ) {
  def rws[W, S](implicit F: Functor[F], W: Monoid[W])
  : ReaderWriterStateT[A, W, S, F, B] =
    ReaderWriterStateT {
      case (r, s) => F.map(rd(r))(
        (W.id, _, s)
      )
    }
}

object ReaderT {
  type Reader[A, B] =
  ReaderT[A, Id, B]
}

case class WriterT[W, F[_], A](
                                  wx: F[(W, A)]
                                  ) {
  def rws[R, S](implicit F: Functor[F])
  : ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT {
      case (r, s) => F.map(wx){
        case (w, a) => (w, a, s)
      }
    }
}

object WriterT {
  type Writer[W, A] =
  WriterT[W, Id, A]
}

case class StateT[S, F[_], A](
                                 st: S => F[(A, S)]
                                 ) {
  def rws[W, R](implicit F: Functor[F], W: Monoid[W])
  : ReaderWriterStateT[R, W, S, F, A] =
    ReaderWriterStateT {
      case (r, s) => F.map(st(s)){
        case (a, s) => (W.id, a, s)
      }
    }
}

object StateT {
  type State[S, A] =
  StateT[S, Id, A]
}

case class Id[A](a: A)

object Id {
  implicit val IdFlatMap: FlatMap[Id] =
    new FlatMap[Id] {
      def fmap[A, B](f: A => B) =
        i => Id(f(i.a))
      def bind[A, B](f: A => Id[B]) =
        i => f(i.a)
    }
}

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
  def map[A, B](a: F[A])(f: A => B): F[B] =
    fmap(f)(a)
}

trait FlatMap[F[_]] extends Functor[F] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]
  def flatMap[A, B](a: F[A])(f: A => F[B]): F[B] =
    bind(f)(a)
}

trait Semigroup[A] {
  def op(a1: A, a2: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def id: A
}
