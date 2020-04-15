package xyz.funnycoding.days

import intMachine._
import xyz.funnycoding.domain.solution._
import cats.Eval

object Day02 {
  def mkSol: List[String] => Solution = list => {
    val machine = Parser.parse(list.mkString).updated(2, 2).updated(1, 12)
    Solution(Runner.evaluate(machine).toString(), Runner.nounAndVerb(machine).toString())
  }

  def mkLazySol: List[String] => LazySolution = list => {
    val machine = Eval.later {
      Parser.parse(list.mkString).updated(2, 2).updated(1, 12)
    }
    val first  = machine.map(m => Runner.evaluate(m).toString())
    val second = machine.map(m => Runner.nounAndVerb(m).toString())
    LazySolution(first, second)
  }
}
