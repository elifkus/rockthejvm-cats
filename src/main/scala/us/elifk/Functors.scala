package us.elifk

object Functors {
  import cats.Functor
  import cats.syntax.functor._
  import cats.instances.list._
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.try_._

  trait Tree[+T]

  case class Leaf[+T](value: T) extends Tree[T]

  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      }
  }

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  def do10xshorter[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    container.map(_ * 10)

}
