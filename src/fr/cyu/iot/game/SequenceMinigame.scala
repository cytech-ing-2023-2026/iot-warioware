package fr.cyu.iot.game

import fr.cyu.iot.Msg
import scala.util.Random
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

object SequenceMinigame extends Minigame:

  enum Input:
    case Up, Down, Left, Right, Press, Neutral

    def toImage: String = this match
      case Input.Up => "/public/input/up.png"
      case Input.Down => "/public/input/down.png"
      case Input.Left => "/public/input/left.png"
      case Input.Right => "/public/input/right.png"
      case Input.Press => "/public/control/push.png"
      case Input.Neutral => "/public/control/push.png"

  object Input:
    val pickable: Seq[Input] = Seq(Input.Up, Input.Down, Input.Left, Input.Press)

  case class Model(step: Int, sequence: Seq[Input], currentInput: Input)

  override val name: String = "Repeat the sequence!"

  override def control: Control = Control.Joystick

  override def duration: Long = 5000

  override def init: Model =
    def randomInput(): Input = Input.pickable(Random.nextInt(Input.pickable.size))
    Model(0, Range(0, 4).map(_ => randomInput()), Input.Neutral)

  override def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg]) =
    val input =
      if controller.pressed then Input.Press
      else if controller.x < 0.25 then Input.Left
      else if controller.x > 0.75 then Input.Right
      else if controller.y < 0.25 then Input.Down
      else if controller.y > 0.75 then Input.Up
      else Input.Neutral

    if input == model.currentInput then (model, Cmd.None)
    else if input == Input.Neutral then (model.copy(currentInput = Input.Neutral), Cmd.None)
    else if input == model.sequence(model.step) then (
      model.copy(step = model.step + 1, currentInput = input),
      if model.sequence.sizeIs <= model.step + 1 then Cmd.emit(GameMsg.MinigameFinished(true))
      else Cmd.None
    )
    else (model, Cmd.emit(GameMsg.MinigameFinished(false)))

  override def view(model: Model): Html[Msg] = ul(cls := "steps steps-horizontal")(
    for (input, i) <- model.sequence.zipWithIndex.toList yield
      val stepStatus =
        if i > model.step then ""
        else "step-success"

      li(cls := s"step $stepStatus")(img(cls := "step-icon object-contains m-2", src := input.toImage))
  )