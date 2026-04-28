package fr.cyu.iot

import fr.cyu.iot.game.Game
import tyrian.websocket.WebSocket
import zio.Task
import zio.json.*

case class Model(
  address: String,
  connected: Boolean,
  socket: Option[WebSocket[Task]],
  game: Option[Game],
  lastScore: Option[Int],
  highScore: Option[Int]
):
  def socketEndpoint: String = s"ws://$address"

object Model:
  val default: Model = Model("192.168.12.81:81", false, None, None, None, None)