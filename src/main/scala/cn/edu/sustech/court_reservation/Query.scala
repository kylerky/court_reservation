package cn.edu.sustech.court_reservation

import cats.effect._
import cats.syntax.all._
import cats._

import org.http4s.client.Client
import org.http4s.Uri
import org.http4s.syntax._
import org.http4s._
import org.http4s.implicits._
import org.http4s.headers.Accept
import org.http4s.circe._

import fs2.io.net.Network
import fs2.Stream

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax._

import io.circe.generic.auto._
import io.circe._
import io.circe.syntax._
import io.circe.literal._
import scala.util.Try
import scala.util.Success
import cats.effect.kernel.MonadCancel
import scala.util.control.NonFatal

case class Response[T](
    code: Int,
    data: T,
    msg: String,
    success: Boolean
)
case class CourtInfoData(records: Seq[CourtInfo])
case class CourtInfo(name: String, gymId: String, id: String, isDeleted: Int)

case class TimeSlot(
    time: String,
    endTime: String,
    timeData: Int,
    status: Int,
    customerName: Option[String],
    customerCode: Option[String],
    customerTel: Option[String],
    clientType: Option[String]
):
  val goodTimeSlots = (0 to 24).map(_.toString()).toVector
  def isAvailable: Boolean =
    status == 1
  def isTimeBetween(first: Int, last: Int): Boolean =
    val splits = time.split(":")
    if splits.length != 2 then false
    else
      Try(splits(0).toInt) match
        case Success(t) => (first to last).contains(t)
        case _          => false

given timeSlotJsonDecoder: Decoder[TimeSlot] = Decoder.forProduct8(
  "time",
  "endTime",
  "timeData",
  "status",
  "customerName",
  "customerCode",
  "customerTel",
  "type"
)(TimeSlot.apply)

case class TimeSlotDataPerDay(date: String, timeBlockList: Seq[TimeSlot])
case class TimeSlotData(configList: Seq[TimeSlotDataPerDay])

given courtInfoResponseDecoder[F[_]: Async, T: Decoder]
    : EntityDecoder[F, Response[T]] =
  jsonOf

case class MessageHookData(courts: Seq[MessageHookEntity])
case class MessageHookEntity(name: String, slots: Seq[String])

case class QueryConfig(baseUri: Uri, messageHookUri: Uri, vars: QueryConfigVars)
case class QueryConfigVars(
    targetCourts: Seq[String],
    goodFirstHour: Int,
    goodLastHour: Int,
    reserveUserId: String,
    reserveHour: Int,
    reserveMinute: Int,
    reserveSecond: Int,
    reserveClockOffsetInMillis: Long,
    repeatCount: Int,
    repeatOffsetInMillis: Int,
    zoneOffsetOfHours: Int,
    queryUserId: String,
    gymId: String,
    gymName: String,
    customerTel: String,
    customerId: String,
    userNum: String,
    customerName: String,
    startTime: String,
    endTime: String,
    enableMessageHook: Boolean,
    minSlotsToMessage: Int,
    queryCourtBlackList: Set[String],
    emptyResultSend: Boolean,
    emptyResultMessage: String,
    defaultDaysToPlus: Int
)
case class ReservationResponseData()

case class FailToReserveError() extends Throwable

case class Query(config: QueryConfig):
  val baseUri = config.baseUri
  val queryUserId = config.vars.queryUserId
  val reserveUserId = config.vars.reserveUserId
  val gymId = config.vars.gymId
  val apiPath = baseUri / "api" / "blade-app" / "qywx"
  val courtListReqName = "getAppGroundPageByGymId"
  val courtTimeReqName = "getOrderTimeConfigList"

  def reserve[F[_]: Async: Logger: Parallel](
      reservationDate: String,
      targets: Stream[F, (String, String)]
  )(using
      client: Client[F]
  ): Stream[F, String] =
    targets
      .parEvalMapUnorderedUnbounded { t =>
        reserveEach(
          reservationDate,
          t._1,
          t._2,
          { r =>
            error"Got an error code: ${r.status}"
              *> r
                .as[Response[Option[Unit]]]
                .flatMap { r => error"Got the message: ${r.msg}" }
              *> RuntimeException(
                "Error when reserving a court"
              ).pure
          }
        )
      }
      .attempt
      .collectFirst { case Right(name) => name }

  def getTarget[F[_]: Async: Logger: Parallel](reservationDate: String)(using
      client: Client[F]
  ): F[Seq[(String, String)]] =
    for
      courts <- getCourtList(queryUserId, gymId)
      targets = courts.data.records
        .filter { r =>
          config.vars.targetCourts.contains(r.name)
        }
        .map { r => (r.name, r.id) }
    yield targets

  def reserveEach[F[_]: Async: Logger: Parallel](
      reservationDate: String,
      courtName: String,
      courtId: String,
      handleError: org.http4s.Response[F] => F[Throwable]
  )(using
      client: Client[F]
  ): F[String] =
    val reservePath = apiPath / "saveOrder"
    val reservePathWithParams =
      reservePath.withQueryParam("userid", reserveUserId)
    val startTime = s"${reservationDate} ${config.vars.startTime}"
    val endTime = s"${reservationDate} ${config.vars.endTime}"
    val req = Request[F](method = Method.POST, uri = reservePathWithParams)
      .withEntity(json"""{
               "customerEmail": "",
               "customerId": ${config.vars.customerId},
               "customerName": ${config.vars.customerName},
               "customerTel": ${config.vars.customerTel},
               "groundId": ${courtId},
               "groundName": ${courtName},
               "groundType": "0",
               "gymId": $gymId,
               "gymName": ${config.vars.gymName},
               "isIllegal": "0",
               "messagePushType": "0",
               "startTime": ${startTime},
               "tmpStartTime": ${startTime},
               "endTime": ${endTime},
               "tmpEndTime": ${endTime},
               "orderDate": ${startTime},
               "tmpOrderDate": ${startTime},
               "userNum": ${config.vars.userNum}}""")
    client
      .expectOr[Response[ReservationResponseData]](req) { handleError(_) }
      .as(courtName)

  def query[F[_]: Async: Logger: Parallel](startDate: String)(using
      client: Client[F]
  ): F[Unit] =
    val endDate = startDate
    for
      courts <- getCourtList(queryUserId, gymId)
      records = courts.data.records.filter { r =>
        !config.vars.queryCourtBlackList.contains(r.name)
      }
      courtMap = Map.from(
        records.map { _.id }.zip(records)
      )
      timeSlots <- records.parTraverse { r =>
        extractCourtTimeSlots(startDate, endDate, queryUserId, courtMap)(r.id)
      }
      goodSlots = timeSlots.filter(s =>
        s._2.length >= config.vars.minSlotsToMessage
      )
      _ <-
        if config.vars.enableMessageHook then
          sendNotification[F](
            startDate,
            goodSlots,
            { r =>
              error"Got an error code: ${r.status}"
                *> r.bodyText.evalMap { r => error"$r" }.compile.drain
                *> RuntimeException(
                  "Error in sending Feishu notification"
                ).pure
            }
          )
        else ().pure[F]
    yield ()

  def getCourtList[F[_]: Async: Logger](userId: String, gymId: String)(using
      client: Client[F]
  ): F[Response[CourtInfoData]] =
    val courtListPath = apiPath / courtListReqName
    val req =
      Request[F](
        method = Method.POST,
        uri = courtListPath
      ).withEntity(
        UrlForm(
          "userid" -> userId,
          "gymId" -> gymId,
          "current" -> "1",
          "size" -> "20",
          "startTime" -> "",
          "endTime" -> "",
          "groundId" -> ""
        )
      )
    client.expect(req)

  def extractCourtTimeSlots[F[_]: Async: Logger](
      startTime: String,
      endTime: String,
      userId: String,
      courtMap: Map[String, CourtInfo]
  )(
      courtId: String
  )(using
      client: Client[F]
  ): F[(CourtInfo, Seq[TimeSlot])] =
    getCourtTimeSlots(startTime, endTime, queryUserId)(courtId)
      .map {
        extractAvailableTimeSlots
      }
      .map { (id, slots) =>
        (
          courtMap(id),
          slots.get(0).getOrElse(Seq.empty).filter {
            _.isTimeBetween(config.vars.goodFirstHour, config.vars.goodLastHour)
          }
        )
      }

  def extractAvailableTimeSlots(
      courtId: String,
      resp: Response[TimeSlotData]
  ): (String, Seq[Seq[TimeSlot]]) =
    (courtId, resp.data.configList.map(_.timeBlockList.filter(_.isAvailable)))

  def getCourtTimeSlots[F[_]: Async: Logger](
      startDate: String,
      endDate: String,
      userId: String
  )(
      courtId: String
  )(using
      client: Client[F]
  ): F[(String, Response[TimeSlotData])] =
    val courtTimePath = apiPath / courtTimeReqName
    val courtTimePathWithParams = courtTimePath.withQueryParams(
      Map(
        "groundId" -> courtId,
        "startDate" -> startDate,
        "endDate" -> endDate,
        "userid" -> queryUserId
      )
    )
    client.expect[Response[TimeSlotData]](courtTimePathWithParams).map {
      (courtId, _)
    }

  def sendNotification[F[_]: Async](
      startDate: String,
      slots: Seq[(CourtInfo, Seq[TimeSlot])],
      handleError: org.http4s.Response[F] => F[Throwable]
  )(using client: Client[F]): F[Unit] =
    val prelude =
      s"${startDate} ${config.vars.goodFirstHour}-${config.vars.goodLastHour}\n"
    val message = slots.isEmpty match
      case true =>
        if !config.vars.emptyResultSend then return ().pure[F]
        s"${prelude}${config.vars.emptyResultMessage}"
      case false =>
        s"${prelude}" + slots
          .map { (court, slots) =>
            s"${court.name}: " + slots.map { _.time }.mkString(",")
          }
          .mkString("\n")

    val req = Request[F](
      method = Method.POST,
      uri = config.messageHookUri
    ).withEntity(json"""{"message": ${message}}""")
    client.expectOr[Unit](req) { handleError(_) }
