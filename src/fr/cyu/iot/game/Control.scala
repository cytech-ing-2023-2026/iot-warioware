package fr.cyu.iot.game

enum Control:
  case Arrows
  case UpDown
  case Push
  case Light

  def toImage: String = this match
    case Control.Arrows => "/public/control/up-down.png"
    case Control.UpDown => "/public/control/up-down.png"
    case Control.Push => "/public/control/push.png"
    case Control.Light => "/public/control/light.png"