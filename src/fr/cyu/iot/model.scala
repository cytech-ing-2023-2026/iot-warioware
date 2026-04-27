package fr.cyu.iot

import fr.cyu.iot.game.Game
import tyrian.websocket.WebSocket
import zio.Task
import zio.json.*

case class Model(address: String, connected: Boolean, socket: Option[WebSocket[Task]], game: Option[Game]):
  def socketEndpoint: String = s"ws://$address"

object Model:
  val default: Model = Model("", false, None, None)
