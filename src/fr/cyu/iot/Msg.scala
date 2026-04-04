package fr.cyu.iot

import tyrian.http.Decoder
import tyrian.http.HttpError
import zio.json.*

enum Msg:
  case SetPollingAddress(value: String)
  case SetPolling(value: Boolean)
  case Poll
  case NetworkError(reason: String)
  case Receive(data: Sensors)
  case NoOp

object Msg:
  def decodingFailed(reason: String): Msg =
    Msg.NetworkError(s"Decoding failure: $reason")

  val decoder: Decoder[Msg] = Decoder(
    onResponse = _.body.fromJson[Sensors].fold(Msg.decodingFailed, Msg.Receive.apply),
    onError =
      case HttpError.BadRequest(msg) => Msg.NetworkError(s"Bad request: $msg")
      case HttpError.NetworkError    => Msg.NetworkError("Network issue")
      case HttpError.Timeout         => Msg.NetworkError("Timeout")
  )
