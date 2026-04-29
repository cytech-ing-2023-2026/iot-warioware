package fr.cyu.iot.game

import fr.cyu.iot.Msg
import scala.util.Random
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

object DogPawMinigame extends Minigame:

  case class Model(
    handX: Double,
    handY: Double,
    vx: Double,
    vy: Double,
    pawX: Double,
    pawY: Double,
    prevPressed: Boolean
  )

  override val name: String = "High five the dog!"

  override def control: Control = Control.Joystick

  override def duration: Long = 8000

  override def init: Model =
    // Generate paw position (keep it away from the extreme edges)
    val pX = 20.0 + Random.nextDouble() * 60.0
    val pY = 20.0 + Random.nextDouble() * 60.0
    
    // Generate hand position far from the paw
    val hX = if pX > 50.0 then pX - 40.0 else pX + 40.0
    val hY = if pY > 50.0 then pY - 40.0 else pY + 40.0

    Model(
      handX = hX,
      handY = hY,
      vx = 0.0,
      vy = 0.0,
      pawX = pX,
      pawY = pY,
      prevPressed = true // Start as true to prevent immediate loss if carried over from previous game
    )

  override def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg]) =
    val rawDx = controller.x - 0.5
    val rawDy = controller.y - 0.5
    
    val deadzone = 0.1
    val ax = if math.abs(rawDx) > deadzone then rawDx * 2.0 else 0.0
    val ay = if math.abs(rawDy) > deadzone then rawDy * 2.0 else 0.0

    val accelFactor = 1.0
    val inertia = 0.87

    val nextVx = (model.vx + ax * accelFactor) * inertia
    val nextVy = (model.vy + ay * accelFactor) * inertia

    val nextX = math.max(0.0, math.min(100.0, model.handX + nextVx))
    val nextY = math.max(0.0, math.min(100.0, model.handY + nextVy))

    val finalVx = if nextX <= 0.0 || nextX >= 100.0 then 0.0 else nextVx
    val finalVy = if nextY <= 0.0 || nextY >= 100.0 then 0.0 else nextVy

    val updatedModel = model.copy(
      handX = nextX,
      handY = nextY,
      vx = finalVx,
      vy = finalVy,
      prevPressed = controller.pressed
    )

    // Detect press transition
    if !model.prevPressed && controller.pressed then
      val dist = math.sqrt(math.pow(nextX - model.pawX, 2) + math.pow(nextY - model.pawY, 2))
      if dist < 15.0 then
        (updatedModel, Cmd.emit(GameMsg.MinigameFinished(true)))
      else
        (updatedModel, Cmd.emit(GameMsg.MinigameFinished(false)))
    else
        (updatedModel, Cmd.None)

  override def view(model: Model): Html[Msg] =
    div(
      cls := "relative w-5xl h-full overflow-hidden rounded-xl border-4 border-info",
      styles("background-color" -> "#cdecf7") // Light blue for ice feel
    )(
      // The Dog Paw
      img(
        cls := "absolute object-contain w-24 h-24 transform -translate-x-1/2 translate-y-1/2",
        // Note: Y coordinates normally go down in CSS. Since our Y is 0=Bottom, 100=Top, we map Y to bottom property.
        styles(
          "left" -> s"${model.pawX}%",
          "bottom" -> s"${model.pawY}%"
        ),
        src := "/public/stock/dog-paw.png"
      ),
      // The Target Zone visual indicator (optional but nice for feedback)
      div(
        cls := "absolute border-4 border-dashed border-white rounded-full opacity-50 transform -translate-x-1/2 translate-y-1/2",
        styles(
          "left" -> s"${model.pawX}%",
          "bottom" -> s"${model.pawY}%",
          "width" -> "30%", // 15 unit radius = 30 unit diameter
          // The container's aspect ratio might not be square, so % for height might be an ellipse.
          // In w-full h-full typical divs, it's safer to use hard sizes, or square %.
          // For simplicity, skip dynamic target zone and rely on user estimating paw radius.
          "display" -> "none"
        )
      )(),
      // The Human Hand
      img(
        cls := "absolute object-contain w-24 h-24 transform -translate-x-1/2 translate-y-1/2", // Centers the image exactly at given %
        styles(
          "left" -> s"${model.handX}%",
          "bottom" -> s"${model.handY}%"
        ),
        src := "/public/stock/human-right-hand.png"
      )
    )