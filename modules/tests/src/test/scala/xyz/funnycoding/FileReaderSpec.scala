package xyz.funnycoding

import suite.PureTestSuite
import cats.effect.IO

final class FileReaderSpec extends PureTestSuite {
  forAll { (_: Int) =>
    spec("test this 1") {
      IO { assert(true) }
    }

  }
}
