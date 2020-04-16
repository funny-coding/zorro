package xyz.funnycoding.days

import intMachine._
import xyz.funnycoding.domain.solution._
import cats.Eval

object Day02 {
  def mkSol: List[String] => Solution = list => {
    val machine = Parser.parse(list.mkString).updated(2, 2).updated(1, 12)
    Solution(Runner.evaluate(machine)(1).toString(), Runner.nounAndVerb(machine)(1).toString())
  }

  def mkLazySol: List[String] => LazySolution = list => {
    val machine = Eval.later {
      Parser.parse(list.mkString).updated(2, 2).updated(1, 12)
    }
    val first  = machine.map(m => Runner.evaluate(m)(1).toString())
    val second = machine.map(m => Runner.nounAndVerb(m)(1).toString())
    LazySolution(first, second)
  }
}
