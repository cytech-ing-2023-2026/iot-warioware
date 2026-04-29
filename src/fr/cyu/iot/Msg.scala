package fr.cyu.iot

import fr.cyu.iot.game.GameMsg
import tyrian.websocket.WebSocket
import tyrian.websocket.WebSocketConnect
import tyrian.websocket.WebSocketEvent
import zio.Task
import zio.json.*

enum Msg:
  case SetAddress(value: String)
  case Connecting(socket: WebSocket[Task])
  case Connected
  case Connect
  case NetworkError(reason: String)
  case Disconnect
  case Disconnected(code: Int, reason: String)
  case StartGame
  case EndGame(score: Int)
  case Game(message: GameMsg)
  case NoOp

object Msg:
  case class Joystick(x: Double, y: Double, pressed: Boolean) derives JsonDecoder
  case class Color(lux: Double) derives JsonDecoder
  case class TMG(color: Color) derives JsonDecoder
  case class RawData(joystick: Joystick, tmg: TMG) derives JsonDecoder

  def decodingFailed(reason: String): Msg =
    Msg.NetworkError(s"Decoding failure: $reason")

  def decodeConnect(connect: WebSocketConnect[Task]): Msg = connect match
    case WebSocketConnect.Error(msg)        => Msg.NetworkError(msg)
    case WebSocketConnect.Socket(webSocket) => Msg.Connecting(webSocket)

  def decodeEvent(event: WebSocketEvent): Msg = event match
    case WebSocketEvent.Close(code, reason) => Msg.Disconnected(code, reason)
    case WebSocketEvent.Error(reason)       => Msg.NetworkError(reason)
    case WebSocketEvent.Heartbeat           => Msg.NoOp
    case WebSocketEvent.Open                => Msg.Connected
    case WebSocketEvent.Receive(message) => message.fromJson[RawData].fold(
        reason => Msg.NetworkError(s"Wrong controller data received: $reason"),
        data =>
          val scaledX = (Constant.JoystickMax - data.joystick.y - Constant.JoystickMin) / Constant.JoystickMax
          val scaledY = (Constant.JoystickMax - data.joystick.x - Constant.JoystickMin) / Constant.JoystickMax
          val scaledLux = (data.tmg.color.lux - Constant.LuxMin) / Constant.LuxMax

          Msg.Game(GameMsg.ControllerUpdated(scaledX, scaledY, data.joystick.pressed, scaledLux))
      )
