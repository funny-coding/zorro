package xyz.funnycoding.programs

import xyz.funnycoding.domain.file._
import xyz.funnycoding.domain.solution._
import eu.timepit.refined.api._
import xyz.funnycoding.file.FileReader
import cats.effect.Sync
import cats.implicits._
import xyz.funnycoding.days._
import cats.Eval

object AdventProgram {

  val programLookup: Map[FilePath, List[String] => Solution] = Map(
    fileName("02") -> Day02.mkSol,
    fileName("06") -> Day6.mkSol
  )

  val lazyProgramLookup: Map[FilePath, List[String] => LazySolution] = Map(
    fileName("02") -> Day02.mkLazySol,
    fileName("05") -> Day05.mkLazySol,
    fileName("06") -> Day6.mkLazySol
  )

  def fileName(value: String): FilePath = FilePath(Refined.unsafeApply(value))

  def solve[F[_]: Sync](path: FilePath, f: List[String] => Solution)(implicit reader: FileReader[F]): F[String] =
    for {
      solution <- reader.withFile(path)(x => Sync[F].delay(f(x)))
    } yield s"solution to ${path.pretty} is :\n --- first  ${solution.first} \n--- second ${solution.second}"

  def solveLazy[F[_]: Sync](path: FilePath, f: List[String] => LazySolution)(
      implicit reader: FileReader[F]
  ): F[String] =
    for {
      solution <- reader
                   .withFile(path)(x => Sync[F].delay(f(x)))
                   .handleErrorWith(_ => {
                     Sync[F].pure(LazySolution(Eval.later("file loading error"), Eval.later("file loading error")))
                   })
    } yield s"solution to ${path.pretty} is :\n --- first  ${solution.first.value} \n--- second ${solution.second.value}"
}
