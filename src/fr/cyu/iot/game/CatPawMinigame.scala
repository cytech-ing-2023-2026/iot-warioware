package fr.cyu.iot.game

import fr.cyu.iot.Msg
import scala.util.Random
import tyrian.Cmd
import tyrian.Html
import tyrian.Html.*
import zio.Task

object CatPawMinigame extends Minigame:

  case class Model(
    handX: Double,
    handY: Double,
    handVx: Double,
    handVy: Double,
    catX: Double,
    catY: Double,
    catVx: Double,
    catVy: Double,
    hit: Boolean
  )

  override val name: String = "DO NOT high five the cat!"

  override def control: Control = Control.Joystick

  override def duration: Long = 8000

  override def init: Model =
    // Generate cat position
    val cX = 20.0 + Random.nextDouble() * 60.0
    val cY = 20.0 + Random.nextDouble() * 60.0
    
    // Generate hand position far from the cat
    val hX = if cX > 50.0 then cX - 40.0 else cX + 40.0
    val hY = if cY > 50.0 then cY - 40.0 else cY + 40.0

    Model(
      handX = hX,
      handY = hY,
      handVx = 0.0,
      handVy = 0.0,
      catX = cX,
      catY = cY,
      catVx = 0.0,
      catVy = 0.0,
      hit = false
    )

  override def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg]) =
    if model.hit then (model, Cmd.None)
    else
      // 1. Hand Physics (Ice-like inertia)
      val rawDx = controller.x - 0.5
      val rawDy = controller.y - 0.5
      
      val deadzone = 0.1
      val ax = if math.abs(rawDx) > deadzone then rawDx * 2.0 else 0.0
      val ay = if math.abs(rawDy) > deadzone then rawDy * 2.0 else 0.0

      val accelFactor = 0.5
      val inertia = 0.87

      val nextHandVx = (model.handVx + ax * accelFactor) * inertia
      val nextHandVy = (model.handVy + ay * accelFactor) * inertia

      val nextHandX = math.max(0.0, math.min(100.0, model.handX + nextHandVx))
      val nextHandY = math.max(0.0, math.min(100.0, model.handY + nextHandVy))

      val finalHandVx = if nextHandX <= 0.0 || nextHandX >= 100.0 then 0.0 else nextHandVx
      val finalHandVy = if nextHandY <= 0.0 || nextHandY >= 100.0 then 0.0 else nextHandVy

      // 2. Cat Physics (Pursues the hand, similar inertia but a bit slower to be fair)
      val dx = model.handX - model.catX
      val dy = model.handY - model.catY
      val dist = math.sqrt(dx * dx + dy * dy)
      
      // Normalize direction and apply a small acceleration
      val (catAx, catAy) = if dist > 0 then (dx / dist * 0.15, dy / dist * 0.15) else (0.0, 0.0)

      val nextCatVx = (model.catVx + catAx) * inertia
      val nextCatVy = (model.catVy + catAy) * inertia

      val nextCatX = math.max(0.0, math.min(100.0, model.catX + nextCatVx))
      val nextCatY = math.max(0.0, math.min(100.0, model.catY + nextCatVy))

      val finalCatVx = if nextCatX <= 0.0 || nextCatX >= 100.0 then 0.0 else nextCatVx
      val finalCatVy = if nextCatY <= 0.0 || nextCatY >= 100.0 then 0.0 else nextCatVy

      // 3. Collision Detection
      val collisionDist = math.sqrt(math.pow(nextHandX - nextCatX, 2) + math.pow(nextHandY - nextCatY, 2))
      val isHit = collisionDist < 6.0

      (
        model.copy(
          handX = nextHandX,
          handY = nextHandY,
          handVx = finalHandVx,
          handVy = finalHandVy,
          catX = nextCatX,
          catY = nextCatY,
          catVx = finalCatVx,
          catVy = finalCatVy,
          hit = isHit
        ),
        Cmd.None
      )

  override def endStatus(model: Model): Option[Boolean] =
    Some(!model.hit)

  override def view(model: Model): Html[Msg] =
    div(
      cls := "relative w-xl h-full overflow-hidden rounded-xl border-4 border-info",
      styles("background-color" -> (if model.hit then "#ffcccc" else "#cdecf7")) // Reddish if hit, otherwise light blue
    )(
      // The Cat Paw
      img(
        cls := "absolute object-contain w-22 h-22 transform -translate-x-1/2 translate-y-1/2",
        styles(
          "left" -> s"${model.catX}%",
          "bottom" -> s"${model.catY}%"
        ),
        src := "/public/stock/cat-paw.png"
      ),
      // The Human Hand
      img(
        cls := "absolute object-contain w-22 h-22 transform -translate-x-1/2 translate-y-1/2",
        styles(
          "left" -> s"${model.handX}%",
          "bottom" -> s"${model.handY}%"
        ),
        src := (if model.hit then "/public/stock/human-right-hand-hurt.png" else "/public/stock/human-right-hand.png")
      )
    )