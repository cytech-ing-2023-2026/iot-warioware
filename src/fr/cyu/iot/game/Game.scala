package fr.cyu.iot.game

import fr.cyu.iot.Msg
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

case class Game(round: Int, health: Int, maxHealth: Int)

object Game:

  val init: Game = Game(0, 3, 4)

  def update(game: Game): GameMsg => (Game, Cmd[Task, GameMsg]) =
    case GameMsg.ControllerUpdated(x, y, pressed, lux) => (game, Cmd.None)
    case GameMsg.MinigameFinished(win)                 => (game, Cmd.None)

  def view(game: Game): Html[Msg] = div(cls := "flex flex-col justify-start items-center")(
    div(cls := "flex flex-row justify-center gap-2")(
      for i <- List.from(0 until game.maxHealth) yield
        img(
          cls := "h-14 w-14 object-contain",
          src := (
            if i < game.health then "/public/heart.png"
            else "/public/heart_empty.png"
          )
        )
      
    )
  )
