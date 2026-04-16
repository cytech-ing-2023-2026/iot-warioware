package fr.cyu.iot

import scala.concurrent.duration.DurationInt
import scala.scalajs.js.annotation.*
import tyrian.*
import tyrian.Html.*
import tyrian.SVG.{pattern as svgPattern, *}
import tyrian.http.Http
import tyrian.http.Request
import zio.*
import zio.json.*
import zio.interop.catz.*

@JSExportTopLevel("TyrianApp")
object Main extends TyrianZIOApp[Msg, Model]:

  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[Task, Msg]) =
    (Model.default, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[Task, Msg]) =
    case Msg.SetPollingAddress(value) => (model.copy(pollAddress = value), Cmd.None)
    case Msg.SetPolling(value)        =>
      val status =
        if value then Status.Neutral(s"Polling from ${model.pollAddress}...")
        else Status.Neutral("Idle")
      (model.copy(polling = value, status = status), Cmd.None)
    case Msg.Poll                     => (model, Http.send(Request.get(model.pollEndpoint), Msg.decoder))
    case Msg.NetworkError(reason)     => (model.copy(polling = false, status = Status.Error(reason)), Cmd.None)
    case Msg.Receive(data)            => (model.copy(sensors = Some(data), status = Status.Success(s"Polling from ${model.pollAddress}...")), Cmd.None)
    case Msg.NoOp                     => (model, Cmd.None)

  def subscriptions(model: Model): Sub[Task, Msg] =
    if model.polling then
      Sub
        .every(1.seconds, "polling")
        .map(_ => Msg.Poll)
    else Sub.None

  def formatTime(uptime: Long): String =
    val hours = uptime / 3_600_000
    val minutes = uptime % 3_600_000 / 60_000
    val seconds = uptime % 60_000 / 1_000

    String.format("%02d:%02d:%02d", hours, minutes, seconds)

  def viewTMG(tmg: TMG): List[Html[Msg]] = List(
    tr(
      td("Proximity"),
      td(tmg.proximity.toString)
    ),
    tr(
      td("Color"),
      td(
        table(cls := "table table-xs")(
          tr(
            td("Red"),
            td(tmg.color.red.toString)
          ),
          tr(
            td("Blue"),
            td(tmg.color.blue.toString)
          ),
          tr(
            td("Green"),
            td(tmg.color.green.toString)
          ),
          tr(
            td("Clear"),
            td(tmg.color.clear.toString)
          ),
          tr(
            td("Lux"),
            td(tmg.color.lux.toString)
          ),
          tr(
            td("CCT"),
            td(tmg.color.cct.toString)
          )
        )
      )
    )
  )

  def viewBME(bme: BME): List[Html[Msg]] = List(
    tr(
      td("Temperature (°C)"),
      td(bme.temperature.toString)
    ),
    tr(
      td("Humidity"),
      td(bme.humidity.toString)
    ),
    tr(
      td("Pressure"),
      td(bme.pressure.toString)
    ),
    tr(
      td("Gas"),
      td(bme.gas.toString)
    )
  )

  def section(name: String): Html[Msg] =
    thead(
      tr(
        th(name)
      )
    )

  def viewSensors(sensors: Sensors): Html[Msg] =
    table(cls := "table table-zebra")(
      List(
        section("General"),
        tr(
          td("Uptime"),
          td(formatTime
      (sensors.uptime))
        ),
        tr(
          td("Heartbeat (mv)"),
          td(sensors.heartbeat.toString)
        ),
        section("BME")
      )
      ++ sensors.bme.fold(Nil)(viewBME)
      ++ List(section("TMG"))
      ++ sensors.tmg.fold(Nil)(viewTMG)
    )

  def status(message: String): Html[Msg] =
    div(cls := "flex flex-col")(
      label(cls := "font-bold")("Status:"),
      p(message)
    )

  def view(model: Model): Html[Msg] =
    div(cls := "w-full h-full flex flex-col justify-start items-center gap-10 py-10")(
      h1(cls := "text-6xl font-bold text-cyan-400")("Sensors monitor"),
      div(cls := "w-5xl h-full flex flex-col justify-start items-center gap-10")(
        div(cls := "w-full flex flex-col items-center gap-2")(
          div(cls := "join")(
            div(cls := "flex flex-col")(
              label(cls := "input validator")(
                svg(cls := "h-[2em] opacity-50", xmlns := "http://www.w3.org/2000/svg", viewBox := "0 0 24 24")(
                  g(
                    Attribute("stroke-linejoin", "round"),
                    Attribute("stroke-linecap", "round"),
                    Attribute("stroke-width", "2.5"),
                    fill := "none",
                    stroke := "currentColor"
                  )(
                    path(d := "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"),
                    path(d := "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71")
                  )
                ),
                label(cls := "opacity-50")("http://"),
                input(
                  `type` := "url",
                  required,
                  placeholder := "127.0.0.1",
                  pattern := """^(https?://)?([a-zA-Z0-9]([a-zA-Z0-9\-].*[a-zA-Z0-9])?\.)+[a-zA-Z].*$""",
                  title := "Must be valid URL",
                  onInput(Msg.SetPollingAddress.apply)
                )
              ),
              p(cls := "validator-hint")("Must be valid URL")
            ),
            button(
              cls := "btn btn-info join-item",
              onClick(Msg.SetPolling(!model.polling))
            )(
              if model.polling then
                if model.sensors.isDefined then span("Stop polling")
                else span(cls := "swap-off loading loading-spinner")("")
              else span("Start polling")
            )
          ),
          model.status match
            case Status.Neutral(message) => div(cls := "alert alert-info alert-outline min-w-md")(status(message))
            case Status.Success(message) => div(cls := "alert alert-success alert-outline min-w-md")(status(message))
            case Status.Error(message)   => div(cls := "alert alert-error alert-outline min-w-md")(status(message))
        ),
        model.sensors.fold(div())(viewSensors)
      )
    )
