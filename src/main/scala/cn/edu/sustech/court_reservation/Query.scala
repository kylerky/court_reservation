package cn.edu.sustech.court_reservation

import cats.effect.Async
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

import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax._

import io.circe.generic.auto._
import io.circe._
import io.circe.syntax._
import io.circe.literal._

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
  val goodTimeSlots = (18 to 22).map(_.toString()).toVector
  def isAvailable: Boolean =
    status == 1
  def isGoodTime: Boolean =
    val splits = time.split(":")
    if splits.length != 2 then false
    else goodTimeSlots.contains(splits(0))

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
    reserveUserId: String,
    reserveHour: Int,
    reserveMinute: Int,
    reserveSecond: Int,
    reserveClockOffsetInMillis: Long,
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
    defaultDaysToPlus: Int
)
case class ReservationResponseData()

case class Query(config: QueryConfig):
  val baseUri = config.baseUri
  val queryUserId = config.vars.queryUserId
  val reserveUserId = config.vars.reserveUserId
  val gymId = config.vars.gymId
  val apiPath = baseUri / "api" / "blade-app" / "qywx"
  val courtListReqName = "getAppGroundPageByGymId"
  val courtTimeReqName = "getOrderTimeConfigList"

  def reserve[F[_]: Async: Logger: Parallel](reservationDate: String)(using
      client: Client[F]
  ): F[Unit] =
    for
      courts <- getCourtList(queryUserId, gymId)
      targets = courts.data.records
        .filter { r =>
          config.vars.targetCourts.contains(r.name)
        }
        .map { r => (r.name, r.id) }
      _ <- targets.parTraverse { t =>
        reserveEach(reservationDate, t._1, t._2)
          .handleErrorWith(e => error"$e" >> Monad[F].pure(()))
      }
    yield ()

  def reserveEach[F[_]: Async: Logger: Parallel](
      reservationDate: String,
      courtName: String,
      courtId: String
  )(using
      client: Client[F]
  ): F[Unit] =
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
    client.expect[Response[ReservationResponseData]](req).as(())

  def query[F[_]: Async: Logger: Parallel](startTime: String)(using
      client: Client[F]
  ): F[Unit] =
    val endTime = startTime
    for
      courts <- getCourtList(queryUserId, gymId)
      courtMap = Map.from(
        courts.data.records.map(_.id).zip(courts.data.records)
      )
      timeSlots <- courts.data.records.parTraverse { r =>
        extractCourtTimeSlots(startTime, endTime, queryUserId, courtMap)(r.id)
      }
      goodSlots = timeSlots.filter(s => s._2.length != 0)
      _ <- sendNotification(startTime, goodSlots)
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
          slots.get(0).getOrElse(Seq.empty).filter { _.isGoodTime }
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
      startTime: String,
      slots: Seq[(CourtInfo, Seq[TimeSlot])]
  )(using client: Client[F]): F[Unit] =
    if slots.isEmpty then return Monad[F].pure(())
    val message = s"$startTime\n" + slots
      .map { (court, slots) =>
        s"${court.name}: " + slots.map { _.time }.mkString(",")
      }
      .mkString("\n")
    val req = Request[F](
      method = Method.POST,
      uri = config.messageHookUri
    ).withEntity(json"""{"message": ${message}}""")
    client.expect[Unit](req)
