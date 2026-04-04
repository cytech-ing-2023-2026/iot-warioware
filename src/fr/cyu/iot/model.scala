package fr.cyu.iot

import zio.json.*

case class Color(red: Double, green: Double, blue: Double, clear: Double, lux: Int, cct: Int) derives JsonDecoder
case class TMG(proximity: Double, color: Color) derives JsonDecoder
case class BME(temperature: Double, humidity: Double, pressure: Double, gas: Double) derives JsonDecoder
case class Sensors(uptime: Long, heartbeat: Int, bme: Option[BME], tmg: Option[TMG]) derives JsonDecoder

enum Status:
  case Neutral(message: String)
  case Success(message: String)
  case Error(reason: String)

case class Model(pollAddress: String, polling: Boolean, status: Status, sensors: Option[Sensors]):
  def pollEndpoint: String = s"http://$pollAddress"

object Model:
  val default: Model = Model("", false, Status.Neutral("Idle"), None)

  //Only used for debugging
  val dummy: Model = Model(
    pollAddress = "",
    polling = false,
    status = Status.Neutral("Idle"),
    sensors = Some(Sensors(
      uptime = 3_700_000,
      heartbeat = 10,
      bme = Some(BME(
        temperature = 25.0,
        humidity = 0.6,
        pressure = 1,
        gas = 0.4
      )),
      tmg = Some(TMG(
        proximity = 5,
        color = Color(255, 255, 0, 100, 10, 5)
      ))
    ))
  )
