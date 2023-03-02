package com.olibipin
package views

import scalafx.scene.control.{ButtonType, Dialog, Label}

class GameOverDialog extends Dialog[ButtonType] {
  private val label = new Label()
  this.getDialogPane.setContent(label)
  this.getDialogPane.getButtonTypes.add(ButtonType.OK)

  def show(winner: String): Boolean = {
    label.setText(s"${winner} won the game!")
    this.showAndWait() match {
      case Some(v) => true
      case _ => false
    }
  }
}
