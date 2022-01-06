package us.elifk

import cats.data.EitherT
import cats.instances.future._

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT(Future(Left(s"Server $server unreachable")))
    case Some(b) => EitherT(Future(Right(b)))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    a <- getBandwidth(s1)
    b <- getBandwidth(s2)
  } yield a + b > 250

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform{
      case Left(reason) => Left(s"Cannot cope with incoming spike. Reason: $reason")
      case Right(false) => Left(s"Cannot cope with incoming spike. Reason: not enough total bandwidth")
      case Right(true) => Right(s"Can copw with incoming spike.")
    }




}
