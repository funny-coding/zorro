package xyz.funnycoding.programs

import xyz.funnycoding.domain.file._
import xyz.funnycoding.domain.solution._
import eu.timepit.refined.api._
import xyz.funnycoding.file.FileReader
import cats.effect.Sync
import cats.implicits._
import xyz.funnycoding.days._

object AdventProgram {

  val programLookup: Map[FilePath, List[String] => Solution] = Map(
    fileName("06") -> Day6.mkSol,
    fileName("02") -> Day02.mkSol
  )

  def fileName(value: String): FilePath = FilePath(Refined.unsafeApply(value))

  def solve[F[_]: Sync](path: FilePath, f: List[String] => Solution)(implicit reader: FileReader[F]): F[String] =
    for {
      solution <- reader.withFile(path)(x => Sync[F].delay(f(x)))
    } yield "solution to " ++ path.pretty ++ " is :\n" ++ "--- first " ++ solution.first ++ "\n--- second " ++ solution.second
}
