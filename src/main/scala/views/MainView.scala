package com.olibipin
package views

import controllers.BoardController
import javafx.scene.layout.VBox
import scalafx.geometry.Insets
import scalafx.scene.control.Label
import scalafx.scene.layout.BorderPane

class MainView(val controller: BoardController) extends BorderPane {
  val gameInfo = {
    val label = new Label("Tic Tac Toe")
    label.setPadding(Insets(20, 0, 10, 20))
    label.setStyle("-fx-font: 32 arial;")
    label
  }
  this.setCenter(new VBox(
    gameInfo,
    new DefaultBoardView(controller)
  ))
}
