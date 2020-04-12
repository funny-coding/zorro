package xyz.funnycoding

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger
import xyz.funnycoding.modules.HttpApi
import org.http4s.server.blaze.BlazeServerBuilder

object Main extends IOApp {

  implicit val logger = Slf4jLogger.getLogger[IO]

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(println("IO here"))
      _ <- Logger[IO].warn("this is a warning")
      api <- HttpApi.make[IO]
      _ <- BlazeServerBuilder[IO]
            .bindHttp(
              8080,
              "0.0.0.0"
            )
            .withHttpApp(api.httpApp)
            .serve
            .compile
            .drain
    } yield ExitCode.Success

}
