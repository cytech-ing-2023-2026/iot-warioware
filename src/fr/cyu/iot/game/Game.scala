package fr.cyu.iot.game

import fr.cyu.iot.Msg
import scala.util.Random
import scala.concurrent.duration.DurationLong
import tyrian.*
import tyrian.Html.*
import zio.Task
import zio.interop.catz.*
import fr.cyu.iot.Model

case class Game(
  round: Int,
  health: Int,
  maxHealth: Int,
  currentMinigame: Minigame,
  currentMinigameState: Any,
  minigameDuration: Long,
  remainingTime: Long
)

object Game:

  val timerGranularity: Long = 16

  private val minigames: List[Minigame] = List(
    ShakeMinigame
  )

  private def randomMinigame(): Minigame = minigames(Random.nextInt(minigames.size))

  def initRandomMinigame(): Game =
    val minigame = randomMinigame()
    Game(1, 3, 4, minigame, minigame.init, minigame.duration, minigame.duration + 1500)

  def nextRound(game: Game): Game =
    val minigame = randomMinigame()
    val durationCoef = 1 + game.round / 4 * 0.1
    val minigameDuration = (minigame.duration / durationCoef).toLong
    game.copy(
      round = game.round + 1,
      currentMinigame = minigame,
      currentMinigameState = minigame.init,
      minigameDuration = minigameDuration,
      remainingTime = minigameDuration + 1500
    )

  def update(game: Game): GameMsg => (Game, Cmd[Task, Msg]) =
    case controller: GameMsg.ControllerUpdated =>
      if game.remainingTime <= game.minigameDuration then
        val current = game.currentMinigame
        val (newState, cmd) = current.update(game.currentMinigameState.asInstanceOf[current.Model], controller)
        (game.copy(currentMinigameState = newState), cmd.map(Msg.Game.apply))
      else
        (game, Cmd.None)
    case GameMsg.MinigameFinished(true) => (nextRound(game), Cmd.None)
    case GameMsg.MinigameFinished(false) =>
      if game.health == 1 then (game, Cmd.emit(Msg.EndGame(game.round)))
      else (nextRound(game.copy(health = game.health - 1)), Cmd.None)
    case GameMsg.TimerDecrement =>
      (
        if game.remainingTime > 0 then game.copy(remainingTime = game.remainingTime - timerGranularity)
        else game,
        if game.remainingTime > 0 && game.remainingTime <= timerGranularity then Cmd.emit(Msg.Game(GameMsg.MinigameFinished(false)))
        else Cmd.None
      )

  def subscriptions(game: Game): Sub[Task, Msg] =
    Sub.every(timerGranularity.milliseconds, "timer").map(_ => Msg.Game(GameMsg.TimerDecrement))

  def view(game: Game): Html[Msg] =
    val progressRatio = math.min(game.remainingTime, game.minigameDuration) * 100 / game.minigameDuration
    val progressStatus =
      if game.remainingTime > game.minigameDuration then "progress-neutral"
      else if game.remainingTime > game.minigameDuration / 2 then "progress-success"
      else if game.remainingTime > game.minigameDuration / 4 then "progress-warning"
      else "progress-error"

    div(cls := "w-full flex flex-col justify-start items-center gap-5")(
      progress(cls := s"progress $progressStatus", value := progressRatio.toString, max := "100")(),
      div(cls := "flex flex-row justify-center gap-4")(
        for i <- List.from(0 until game.maxHealth) yield
          img(
            cls := "h-14 w-14 object-contain",
            src := (
              if i < game.health then "/public/heart.png"
              else "/public/heart_empty.png"
            )
          )
      ),
      h2(cls := "text-xl")(s"Round ${game.round}"),
      h2(cls := "text-2xl")(game.currentMinigame.name),
      div(cls := "h-100 flex flex-col justify-start")(
        game.currentMinigame.view(game.currentMinigameState.asInstanceOf[game.currentMinigame.Model])
      )
    )
