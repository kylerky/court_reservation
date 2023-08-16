package cn.edu.sustech.court_reservation

import cats.effect.{IO, IOApp}
import cats.syntax._
import org.typelevel.log4cats.slf4j._
import org.typelevel.log4cats._

import org.http4s.ember.client.EmberClientBuilder
import org.http4s.syntax.all._
import org.http4s.client.Client
import org.http4s.Uri

import java.nio.file.FileSystems
import fs2.io.file.Path
import fs2.io.file.Files
import fs2.Stream

import fs2.data.json.codec._
import fs2.data.json._
import fs2.data.json.circe._

import io.circe.generic.auto._
import io.circe.syntax._

import com.github.nscala_time.time.Imports._

given logger: SelfAwareStructuredLogger[IO] = Slf4jFactory.create[IO].getLogger

case class Config(
    baseUri: String,
    messageHookUri: String,
    vars: QueryConfigVars
)

object Main extends IOApp.Simple:
  val defaultFs = FileSystems.getDefault()
  val configPath = Path.fromFsPath(defaultFs, "court_reservation.json")

  val getConfig =
    Files[IO]
      .readUtf8(configPath)
      .through(tokens[IO, String])
      .through(deserialize[IO, Config])
      .flatMap(config =>
        val queryConfig = for
          baseUri <- Uri.fromString(config.baseUri)
          messageHookUri <- Uri.fromString(config.messageHookUri)
        yield QueryConfig(baseUri, messageHookUri, config.vars)
        Stream.fromEither[IO](queryConfig)
      )
      .compile
      .last

  val dateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")

  val run =
    for
      maybeConfig <- getConfig
      config <- IO.fromOption(maybeConfig)(
        RuntimeException("Cannot load the configuration")
      )
      _ <- EmberClientBuilder
        .default[IO]
        .build
        .use { c =>
          given client: Client[IO] = c
          Query(config).query[IO](dateFormat.print(DateTime.now() + 7.days))
        }
    yield ()
