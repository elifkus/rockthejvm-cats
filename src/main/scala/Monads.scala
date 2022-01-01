package us.elifk

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {
  val numbers = List(1, 2, 3)
  val chars = List('a', 'b', 'c')

  val combinedList: List[(Int, Char)] = numbers.flatMap(a => chars.map(b => (a,b)))

  val combinedListFor = for {
    n <- numbers
    c <- chars
  } yield (n,c)

  val numberOption = Option(2)
  val charOption = Option('d')

  val combinedOption = numberOption.flatMap(a => charOption.map(b => (a, b)))
  val combinedOptionFor = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(43)
  val charFuture = Future('z')

  val combinedFuture = numberFuture.flatMap(a => charFuture.map(b => (a, b)))
  val combinedFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  import cats.instances.future._
  val futureMonad = Monad[Future]
  val futurePure = futureMonad.pure(42)
  val aTransformedFutureMonad = futureMonad.flatMap(futurePure)(a => Future(a + 43))

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  import cats.Monad
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)


  def main(args: Array[String]): Unit = {

    println(combinedList)
    println(combinedListFor)
  }
}
