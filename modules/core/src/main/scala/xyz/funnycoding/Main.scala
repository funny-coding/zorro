package xyz.funnycoding

import cats.effect._
import io.chrisdavenport.log4cats.slf4j._
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
            val x = lazyProgramLookup.toList
              .parTraverse {
                case (key, value) => solveLazy[IO](key, value).flatMap(putStrLn)
              }

            putStrLn("empty shit, gonna run all") *> x
          } else {
            val path = fileName(args.head)
            lazyProgramLookup.get(path) match {
              case None => putStrLn("unexisting puzzle")
              case Some(f) => {
                solveLazy[IO](path, f).flatMap(putStrLn)
              }
            }
          }
    } yield ExitCode.Success

  def putStrLn(str: String): IO[Unit] = IO { println(str) }

}
