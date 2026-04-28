package fr.cyu.iot.game

import fr.cyu.iot.Msg
import fr.cyu.iot.Constant
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

object ShakeMinigame extends Minigame:
  enum Position:
    case Up, Down, Start

  object Position:
    def fromY(y: Double): Option[Position] =
      if y > 0.75 then Some(Position.Up)
      else if y < 0.25 then Some(Position.Down)
      else None

  case class Model(remaining: Int, position: Position, y: Double)

  override val name: String = "Shake the bottle!"

  override val duration: Long = 4000

  override val init: Model = Model(10, Position.Start, 0.5)

  override def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg]) =
    val position = Position.fromY(controller.y)
    val remaining = model.remaining - position.fold(0)(pos => if pos == model.position then 0 else 1)

    (
      model.copy(
        remaining = remaining,
        position = position.getOrElse(model.position),
        y = controller.y
      ),
      if remaining == 0 then Cmd.emit(GameMsg.MinigameFinished(true))
      else Cmd.None
    )

  override def view(model: Model): Html[Msg] =
    val translation = ((0.5 - model.y) * 125).toInt
    div(cls := "h-full flex flex-col justify-center")(
      img(
        cls := "h-40 w-40 object-contain",
        styles(
          "translate" -> s"0% $translation%",
          "transition" -> "translate 0.1s"
        ),
        src := "/public/bottle.png"  
      )
    )
