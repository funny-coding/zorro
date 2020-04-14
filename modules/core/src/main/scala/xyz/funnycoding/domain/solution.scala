package xyz.funnycoding.domain

import cats.Eval

object solution {
  case class Solution(first: String, second: String)
  case class LazySolution(first: Eval[String], second: Eval[String])
}
