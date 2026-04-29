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
  remainingTime: Long,
  lastOutcome: Game.Outcome
)

object Game:

  enum Outcome:
    case Start, Loss, Close, Win

  val waitTime = 2000

  val timerGranularity: Long = 16

  private val minigames: List[Minigame] = List(
    ShakeMinigame,
    LightMinigame,
    SequenceMinigame
  )

  private def randomMinigame(): Minigame = minigames(Random.nextInt(minigames.size))

  def initRandomMinigame(): Game =
    val minigame = randomMinigame()
    Game(1, 4, 4, minigame, minigame.init, minigame.duration, minigame.duration + waitTime, Outcome.Start)

  def nextRound(game: Game): Game =
    val minigame = randomMinigame()
    val durationCoef = math.pow(0.85, game.round / 4)
    val minigameDuration = (minigame.duration / durationCoef).toLong
    game.copy(
      round = game.round + 1,
      currentMinigame = minigame,
      currentMinigameState = minigame.init,
      minigameDuration = minigameDuration,
      remainingTime = minigameDuration + waitTime
    )

  def update(game: Game): GameMsg => (Game, Cmd[Task, Msg]) =
    case controller: GameMsg.ControllerUpdated =>
      if game.remainingTime <= game.minigameDuration then
        val current = game.currentMinigame
        val (newState, cmd) = current.update(game.currentMinigameState.asInstanceOf[current.Model], controller)
        (game.copy(currentMinigameState = newState), cmd.map(Msg.Game.apply))
      else
        (game, Cmd.None)
    case GameMsg.MinigameFinished(true) =>
      val outcome =
        if game.remainingTime.toDouble / game.minigameDuration < 0.25 then Outcome.Close
        else Outcome.Win
      (nextRound(game.copy(lastOutcome = outcome)), Cmd.None)
    case GameMsg.MinigameFinished(false) =>
      if game.health == 1 then (game, Cmd.emit(Msg.EndGame(game.round)))
      else (nextRound(game.copy(health = game.health - 1, lastOutcome = Outcome.Loss)), Cmd.None)
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

    val currentWait = game.remainingTime - game.minigameDuration
    val waitProgress = currentWait * 100 / waitTime
    val waitSeconds = currentWait / 1000

    val gameView = game.currentMinigame.view(game.currentMinigameState.asInstanceOf[game.currentMinigame.Model])

    val lastOutcomeColor = game.lastOutcome match
      case Outcome.Start => "text-neutral"
      case Outcome.Loss => "text-error"
      case Outcome.Close | Outcome.Win => "text-success"

    val lastOutcomeText =
      if game.round % 4 == 0 then "Faster !"
      else
        game.lastOutcome match
          case Outcome.Start => "Good luck!"
          case Outcome.Loss => "Oh no..."
          case Outcome.Close => "Close."
          case Outcome.Win => "Nice!"

    val boldIfFaster =
      if game.round % 4 == 0 then "font-bold"
      else ""

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
      div(cls := "flex flex-row justify-center items-center gap-2")(
        h2(cls := "text-2xl")(game.currentMinigame.name),
        img(cls := "w-6 h-6 object-contain", src := game.currentMinigame.control.toImage)
      ),
      div(cls := "h-100 flex flex-col justify-start")(
        if game.remainingTime > game.minigameDuration then
          div(cls := "h-full stack")(
            div(
              cls := "h-full w-full bg-white/90 flex flex-col justify-center items-center gap-5"
            )(
              p(cls := s"text-2xl $boldIfFaster $lastOutcomeColor")(lastOutcomeText),
              div(
                cls := s"radial-progress $lastOutcomeColor text-2xl",
                styles(
                  "--value" -> waitProgress.toString,
                  "--size" -> "8rem"
                ),
                attr("aria-valuenow") := waitProgress.toString,
                role := "progressbar"
              )(s"${waitSeconds}s")
            ),
            gameView
          )
        else
          gameView
      )
    )
