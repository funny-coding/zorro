package suite

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import cats.effect.ContextShift
import cats.effect.IO
import scala.concurrent.ExecutionContext
import cats.effect.Timer
import java.{ util => ju }
import org.scalatest.compatible.Assertion
import org.scalactic.source.Position

trait PureTestSuite extends AsyncFunSuite with ScalaCheckDrivenPropertyChecks {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO]     = IO.timer(ExecutionContext.global)

  private def mkUnique(name: String): String =
    s"$name - ${ju.UUID.randomUUID}"

  def spec(testName: String)(f: => IO[Assertion])(implicit pos: Position): Unit =
    test(mkUnique(testName))(IO.suspend(f).unsafeToFuture())
}
