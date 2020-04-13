package xyz.funnycoding.file

import cats.effect.Sync
import scala.io._
import cats.implicits._
import xyz.funnycoding.domain.file._

trait FileReader[F[_]] {
  def withFile[A](path: FilePath)(f: List[String] => F[A]): F[A]
}

object FileReader {}

final class LiveReader[F[_]: Sync] extends FileReader[F] {

  private def openFile(path: FilePath): F[BufferedSource]       = Sync[F].delay(Source.fromResource(path.name))
  private def line(fileStream: BufferedSource): F[List[String]] = Sync[F].delay(fileStream.getLines().toList)
  override def withFile[A](path: FilePath)(f: List[String] => F[A]): F[A] =
    for {
      fileStream <- openFile(path)
      data <- line(fileStream)
      result <- f(data)
    } yield result
}
