package fr.cyu.iot.game

import fr.cyu.iot.Msg
import tyrian.Cmd
import tyrian.Html
import zio.Task

trait Minigame:

  type Model

  def name: String

  def control: Control

  def duration: Long

  def init: Model

  def update(model: Model, controller: GameMsg.ControllerUpdated): (Model, Cmd[Task, GameMsg])

  def endStatus(model: Model): Boolean = false

  def view(model: Model): Html[Msg]
