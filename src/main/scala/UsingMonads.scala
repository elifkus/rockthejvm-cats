package us.elifk

import scala.util.{Failure, Try}

object UsingMonads {

  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost"
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(config: Map[String, String]): M[Connection]
    def issueRequest(conn: Connection, payload: String): M[String]
  }


  object TryHttpService extends HttpService[Try] {
    override def getConnection(config: Map[String, String]): Try[Connection] =
      if (config.contains("host") && config.contains("port")) Try(Connection(config("host"), config("port"))) else Failure(new Exception("Config info not found"))

    override def issueRequest(conn: Connection, payload: String): Try[String] =
      if (payload.size < 20) Try("request (payload) has been accepted") else Failure(new Exception("Failed: Payload longer than 20"))
  }

  type LoadingOr[T] = Either[String, T]

  object LoadingOrHttpService extends HttpService[LoadingOr] {
    override def getConnection(config: Map[String, String]): LoadingOr[Connection] =
      if (config.contains("host") && config.contains("port")) Right(Connection(config("host"), config("port"))) else Left("Config info not found")

    override def issueRequest(conn: Connection, payload: String): LoadingOr[String] =
      if (payload.size < 20) Right("request (payload) has been accepted") else Left("Failed: Payload longer than 20")
  }

  def main(args: Array[String]): Unit = {

  }
}
