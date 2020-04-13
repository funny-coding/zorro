package xyz.funnycoding

import cats.effect.IOApp
import cats.effect.{ ExitCode, IO }
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import xyz.funnycoding.file._
import xyz.funnycoding.programs.AdventProgram._
import cats.effect._
import cats.implicits._

object Main extends IOApp {

  implicit val logger = Slf4jLogger.getLogger[IO]
  implicit val reader = new LiveReader[IO]
  override def run(args: List[String]): IO[ExitCode] =
    for {
      a <- args.pure[IO]
      _ <- if (a.isEmpty) {
            val x = programLookup.toList
              .traverse {
                case (key, value) => solve[IO](key, value).flatMap(putStrLn)
              }

            putStrLn("empty shit, gonna run all") *> x
          } else {
            val path = fileName(args.head)
            programLookup.get(path) match {
              case None => putStrLn("unexisting puzzle")
              case Some(f) => {
                solve[IO](path, f).flatMap(putStrLn)
              }
            }
          }
    } yield ExitCode.Success

  def putStrLn(str: String): IO[Unit] = IO { println(str) }

}
