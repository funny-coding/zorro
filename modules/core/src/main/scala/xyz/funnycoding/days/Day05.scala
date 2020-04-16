package xyz.funnycoding.days

import xyz.funnycoding.domain.solution.LazySolution
import cats.Eval
import xyz.funnycoding.days.intMachine._

object Day05 {
  def mkLazySol: List[String] => LazySolution = list => {
    val machine = Eval.later {
      Parser.parse(list.mkString)
    }.memoize
    val first  = machine.map(m => Runner.outputs(m)(1).toString())
    val second = machine.map(m => Runner.outputs(m)(5).toString())
    LazySolution(first, second)
  }
}
