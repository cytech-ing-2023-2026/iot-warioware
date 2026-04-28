package fr.cyu.iot.game

import fr.cyu.iot.Msg
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task
import scala.util.Random

object LightMinigame extends Minigame:

  case class Model(targetLux: Double, currentLux: Double, remaining: Long, timer: Long)

  def isInRange(targetLux: Double, currentLux: Double): Boolean = math.abs(targetLux - currentLux) < 0.1

  override def name: String = "Adjust the light..."

  override def duration: Long = 10_000

  override def init: Model = Model(Random.between(0.125, 0.875), 0, 1000, 1000)

  override def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg]) =
    val remaining =
      if isInRange(model.targetLux, controller.lux) then model.remaining - 16
      else model.timer

    (
      model.copy(currentLux = controller.lux, remaining = remaining),
      if remaining <= 0 then Cmd.emit(GameMsg.MinigameFinished(true))
      else Cmd.None 
    )

  override def view(model: Model): Html[Msg] =
    val distance = math.abs(model.targetLux - model.currentLux)
    val progressStatus =
      if distance < 0.125 then "progress-success"
      else if distance < 0.25 then "progress-neutral"
      else if distance < 0.5 then "progress-warning"
      else "progress-error"

    val timerOpacity =
      if isInRange(model.targetLux, model.currentLux) then "1"
      else "0"

    div(cls := "w-2xl flex flex-col gap-2")(
      progress(cls := s"progress $progressStatus", value := (model.currentLux * 100).toString, max := "100")(),
      div(cls := "flex flex-col items-center", style("opacity", timerOpacity))(
        progress(
          cls := "progress progress-neutral",
          value := (model.timer - model.remaining).toString,
          max := model.timer.toString
        )(),
        p(cls := "text-xl")("Hold it...")
      )
    )