package fr.cyu.iot.game

enum GameMsg:
  case StartGame
  case MinigameFinished(win: Boolean)