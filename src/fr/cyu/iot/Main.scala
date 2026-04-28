package fr.cyu.iot

import fr.cyu.iot.game.Game
import scala.concurrent.duration.DurationInt
import scala.scalajs.js.annotation.*
import tyrian.*
import tyrian.Html.*
import tyrian.SVG.{pattern as svgPattern, *}
import tyrian.http.Http
import tyrian.http.Request
import tyrian.websocket.WebSocket
import zio.*
import zio.interop.catz.*
import zio.json.*

@JSExportTopLevel("TyrianApp")
object Main extends TyrianZIOApp[Msg, Model]:

  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[Task, Msg]) =
    (Model.default, Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[Task, Msg]) =
    case Msg.SetAddress(value)  => (model.copy(address = value), Cmd.None)
    case Msg.Connecting(socket) => (model.copy(socket = Some(socket)), Cmd.None)
    case Msg.Connected          => (model.copy(connected = true), Cmd.None)
    case Msg.Connect            => (model, WebSocket.connect(model.socketEndpoint)(Msg.decodeConnect))
    case Msg.NetworkError(reason) =>
      println(s"Network error: $reason")
      (model.copy(connected = false, socket = None), Cmd.None)
    case Msg.Disconnected(1000, _)      => (model.copy(connected = false, socket = None), Cmd.None)
    case Msg.Disconnected(code, reason) => (model.copy(connected = false, socket = None), Cmd.None)
    case Msg.StartGame                  => (model.copy(game = Some(Game.initRandomMinigame())), Cmd.None)
    case Msg.EndGame(score)             => (
      model.copy(
        game = None,
        lastScore = Some(score),
        highScore = model.highScore.fold(Some(score))(s => Some(math.max(s, score)))
      ),
      Cmd.None
    )
    case Msg.Game(message) => model.game.fold((model, Cmd.None))(game =>
        val (updated, cmd) = Game.update(game)(message)
        (model.copy(game = Some(updated)), cmd)
      )
    case Msg.NoOp => (model, Cmd.None)

  def subscriptions(model: Model): Sub[Task, Msg] =
    model.socket.fold(Sub.None)(_.subscribe(Msg.decodeEvent)) |+| model.game.fold(Sub.None)(Game.subscriptions)

  def viewMainMenu(model: Model): Html[Msg] = div(cls := "h-full flex flex-col justify-center items-center gap-10")(
    div(cls := "flex flex-col gap-2")(
      model.lastScore.fold(div())(score => p(cls := "text-lg font-bold")(s"Last score: $score")),
      model.highScore.fold(div())(score => p(cls := "text-lg font-bold")(s"High score: $score")),
    ),
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
          label(cls := "opacity-50")("ws://"),
          input(
            `type` := "text",
            required,
            placeholder := "127.0.0.1",
            value := model.address,
            onInput(Msg.SetAddress.apply)
          )
        )
      ),
      button(
        cls := "btn btn-info join-item",
        onClick(Msg.Connect)
      )(
        if model.socket.isDefined then
          if model.connected then span("Change controller")
          else span(cls := "swap-off loading loading-spinner")("")
        else span("Connect to controller")
      )
    ),
    if model.connected then
      button(
        cls := "btn btn-wide btn-success",
        onClick(Msg.StartGame)
      )("Start game")
    else button(cls := "btn btn-wide btn-disabled")("Start game")
  )

  def view(model: Model): Html[Msg] = div(cls := "w-screen h-screen flex flex-col justify-start items-center p-15")(
    h1(
      span(cls := "text-5xl")("WarioWare"),
      span(cls := "text-2xl")("Wish")
    ),
    model.game match
      case Some(game) if model.connected => Game.view(game)
      case _                             => viewMainMenu(model)
  )
