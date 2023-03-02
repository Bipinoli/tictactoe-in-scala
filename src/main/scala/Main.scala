package com.olibipin

import views.MainView

import com.olibipin.controllers.BoardController
import scalafx.application.JFXApp3
import scalafx.scene.Scene

object Main extends JFXApp3 {

  override def start(): Unit = {
    stage = new JFXApp3.PrimaryStage {
      title = "Tic tac toe"
      scene = new Scene {
        root = new MainView(new BoardController)
      }
    }
  }
}
