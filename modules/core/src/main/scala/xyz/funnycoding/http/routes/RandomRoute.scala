package xyz.funnycoding.http.routes

import cats._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

final class RandomRoute[F[_]: Defer: Monad] extends Http4sDsl[F] {

  private[routes] val prefixPath = "/random"

  private val httpRoutes: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root => Ok()
  }

  val routes: HttpRoutes[F] = Router(
    prefixPath -> httpRoutes
  )

}
