package fr.cyu.iot.game

enum GameMsg:
  case ControllerUpdated(x: Double, y: Double, pressed: Boolean, lux: Double)
  case MinigameFinished(win: Boolean)
  case TimerDecrement
