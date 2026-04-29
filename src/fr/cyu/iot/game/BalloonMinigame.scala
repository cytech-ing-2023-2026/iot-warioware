package fr.cyu.iot.game

import fr.cyu.iot.Msg
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

object BalloonMinigame extends Minigame:

  val MaxSize: Double = 1.0
  val WinSize: Double = 0.75

  case class Model(size: Double, popped: Boolean)

  override val name: String = "Blow the balloon!"

  override val control: Control = Control.Push

  override val duration: Long = 6000

  override def init: Model = Model(0.0, false)

  override def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg]) =
    if model.popped then (model, Cmd.None)
    else
      // Increase size if pressed, decrease if not pressed (but not below 0)
      val newSize =
        if controller.pressed then model.size + 0.015
        else math.max(0.0, model.size - 0.005)
      
      if newSize > MaxSize then
        (model.copy(size = newSize, popped = true), Cmd.None)
      else
        (model.copy(size = newSize), Cmd.None)

  override def endStatus(model: Model): Option[Boolean] =
    Some(!model.popped && model.size >= WinSize)

  override def view(model: Model): Html[Msg] =
    val scale = if model.popped then 0.0 else 0.5 + model.size * 1.5
    val balloonOpacity = 1.0 - (model.size * 0.4)
    val progressStatus =
      if model.size > 0.9 then "progress-error"
      else if model.size >= WinSize then "progress-success"
      else if model.size > WinSize - 0.25 then "progress-warning"
      else "progress-error"

    div(cls := "h-full flex flex-col justify-center items-center gap-10 w-full")(
      div(
        cls := "flex flex-col justify-end items-center h-48 w-48"
      )(
        if model.popped then
          img(
            cls := "object-contain",
            src := "/public/explosion.png"
          )
        else
          img(
            cls := "object-contain",
            styles(
              "transform" -> s"scale($scale)",
              "opacity" -> balloonOpacity.toString,
              "transition" -> "transform 0.05s, opacity 0.05s"
            ),
            src := "/public/balloon.png"
          )
      ),
      div(cls := "w-56 flex flex-col items-center gap-1 relative")(
        progress(
          cls := s"progress $progressStatus w-full",
          value := (model.size * 100).toString,
          max := "100"
        )(),
        div(
          cls := "absolute top-0 bottom-0 border-l-2 border-warning",
          styles("left" -> s"${WinSize * 100}%")
        )()
      )
    )