package fr.cyu.iot.game

import fr.cyu.iot.Msg
import tyrian.Cmd
import zio.Task
import tyrian.Html

trait Minigame:

  type Model

  def name: String

  def init: Model

  def update(model: Model): Msg => (Model, Cmd[Task, Msg])

  def view(model: Model): Html[Msg]