package cn.edu.sustech.court_reservation

import cats.effect.{IO, IOApp, ExitCode, Temporal}
import cats.syntax.all._
import org.typelevel.log4cats.slf4j._
import org.typelevel.log4cats._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax._
import org.typelevel.cats.time._

import java.time._
import scala.concurrent.duration._

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
import scala.util.Try

given logger: SelfAwareStructuredLogger[IO] = Slf4jFactory.create[IO].getLogger

case class Config(
    baseUri: String,
    messageHookUri: String,
    vars: QueryConfigVars
)

object Main extends IOApp:
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

  override def run(args: List[String]): IO[ExitCode] =
    for
      maybeConfig <- getConfig
      config <- IO.fromOption(maybeConfig)(
        RuntimeException("Cannot load the configuration")
      )
      isQuery <- args.get(0).getOrElse("-q") match {
        case "-q" => true.pure[IO]
        case "-r" => false.pure[IO]
        case o    => IO.raiseError(RuntimeException(s"Unknown option ${o}"))
      }
      daysToPlus <- args.get(1) match {
        case Some(days) => IO.fromTry(Try(days.toInt))
        case None       => config.vars.defaultDaysToPlus.pure[IO]
      }
      _ <- EmberClientBuilder
        .default[IO]
        .build
        .use { c =>
          given client: Client[IO] = c
          val today = LocalDate.now
          val targetDay = today.plusDays(daysToPlus)
          if isQuery then Query(config).query[IO](targetDay.show)
          else reserve(today, targetDay, config)
        }
    yield ExitCode.Success

  def reserve(today: LocalDate, targetDay: LocalDate, config: QueryConfig)(using
      Client[IO]
  ): IO[Unit] =
    val repeatOffsets = (0 to config.vars.repeatCount - 1).map {
      _.millis * config.vars.repeatOffsetInMillis
    }.toList
    val reserveTime = LocalDateTime.of(
      today,
      LocalTime.of(
        config.vars.reserveHour,
        config.vars.reserveMinute,
        config.vars.reserveSecond
      )
    )
    val maxPar = 5
    val reserveInstant =
      reserveTime.toInstant(ZoneOffset.ofHours(config.vars.zoneOffsetOfHours))
    val query = Query(config)
    for
      targets <- query.getTarget[IO](targetDay.show)
      _ <- Stream
        .emits(repeatOffsets)
        .covary[IO]
        .parEvalMap(maxPar) { t =>
          IO(Instant.now)
            .map { now =>
              val sleepDuration = java.time.Duration
                .between(now, reserveInstant)
                .toNanos()
                .nanos + config.vars.reserveClockOffsetInMillis.millis
              query.reserve[IO](
                targetDay.show,
                Stream.emits(targets).delayBy(sleepDuration)
              )
            }
        }
        .parJoin(maxPar)
        .take(1)
        .compile
        .last
        .flatMap {
          case None       => info"Fail to reserve a court"
          case Some(name) => info"Reserved $name"
        }
    yield ()
