package xyz.funnycoding.modules

import cats.effect.Timer
import cats.effect.Concurrent
import xyz.funnycoding.http.routes.RandomRoute
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.middleware._
import scala.concurrent.duration._
import cats.effect.Sync

object HttpApi {
  def make[F[_]: Timer: Concurrent]: F[HttpApi[F]] = Sync[F].delay(new HttpApi)
}
final class HttpApi[F[_]: Timer: Concurrent] {

  private val randomRoute = new RandomRoute[F].routes

  private val middleware: HttpRoutes[F] => HttpRoutes[F] = {
    { http: HttpRoutes[F] =>
      AutoSlash(http)
    } andThen { http: HttpRoutes[F] =>
      CORS(http, CORS.DefaultCORSConfig)
    } andThen { http: HttpRoutes[F] =>
      Timeout(60.seconds)(http)
    }
  }

  private val loggers: HttpApp[F] => HttpApp[F] = {
    { http: HttpApp[F] =>
      RequestLogger.httpApp(true, true)(http)
    } andThen { http: HttpApp[F] =>
      ResponseLogger.httpApp(true, true)(http)
    }
  }

  val httpApp: HttpApp[F] = loggers(middleware(randomRoute).orNotFound)

}
