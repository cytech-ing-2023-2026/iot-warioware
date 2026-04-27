package fr.cyu.iot.game

import fr.cyu.iot.Msg
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

object ShakeMinigame extends Minigame:
  enum Position:
    case Up, Down, Start

  case class Model(remaining: Int, position: Position, y: Double)

  override val name: String = "Shake the bottle!"

  override val init: Model = Model(10, Position.Start, 0)

  override def update(model: Model): Msg => (Model, Cmd[Task, Msg]) = ???

  override def view(model: Model): Html[Msg] =
    div(cls := "text-xl", style("translate-y", model.y.toString))("Bouteille")
