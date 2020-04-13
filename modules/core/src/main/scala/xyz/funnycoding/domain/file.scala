package xyz.funnycoding.domain

import eu.timepit.refined.api._
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.Size
import eu.timepit.refined.string.ValidInt
import io.estatico.newtype.macros.newtype

object file {
  type FileName = String Refined (Size[2] And ValidInt)
  @newtype case class FilePath(value: FileName) {
    def name: String   = "day" ++ value.value ++ ".txt"
    def pretty: String = "day" ++ value.value
  }
}
