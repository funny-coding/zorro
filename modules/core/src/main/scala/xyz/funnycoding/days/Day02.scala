package xyz.funnycoding.days

import intMachine._
import xyz.funnycoding.domain.solution.Solution

object Day02 {
  def mkSol: List[String] => Solution = list => {
    val machine = Parser.parse(list.mkString).updated(2, 2).updated(1, 12)
    Solution(Runner.evaluate(machine).toString(), Runner.nounAndVerb(machine).toString())
  }
}
