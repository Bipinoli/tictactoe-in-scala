package com.olibipin
package views

import controllers.BoardController

import com.olibipin.observable.{Listener, Subject}
import javafx.scene.layout.VBox
import scalafx.geometry.Insets
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.control.Label
import scalafx.scene.image.Image
import scalafx.scene.layout.{Background, BorderPane, GridPane}
import scalafx.scene.paint.Color

sealed trait BoardView {
  def boardController: BoardController
  def paintCross(row: Int, col: Int): Unit
  def paintCircle(row: Int, col: Int): Unit
  def displayGameOver(winner: String): Unit
  def clearAll(): Unit
  def squareSize: Int = 200
  def gap: Int = 10
}

class BoardViewGrid(val boardController: BoardController) extends GridPane with BoardView {
  this.padding = Insets(gap)
  val gameOverDialog = new GameOverDialog()
  val grid: Array[Array[Canvas]] = {
    (for (i <- 0 to 2) yield {
      (for (j <- 0 to 2) yield {
        val canvas = new Canvas(squareSize + 2 * gap, squareSize + 2 * gap)
        val gc = canvas.graphicsContext2D
        gc.setFill(Color.Gray)
        gc.strokeRect(0, 0, squareSize + 2 * gap, squareSize + 2 * gap)
        //        gc.drawImage(new Image("file:assets/cross.png"), 0, 0, squareSize, squareSize)
        this.add(canvas, i, j)
        canvas
      }).toArray[Canvas]
    }).toArray
  }

  initListeners()

  def initListeners(): Unit = {
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        val canvas = grid(i)(j)
        canvas.onMouseClicked = (e) => {
          boardController.handlePlayAt(i, j, this)
        }
      }
    }
  }

  override def paintCross(row: Int, col: Int): Unit = {
    val gc = grid(row)(col).getGraphicsContext2D
    gc.fillRoundRect(gap, gap, squareSize, squareSize, gap, gap)
  }

  override def paintCircle(row: Int, col: Int): Unit = {
    val gc = grid(row)(col).getGraphicsContext2D
    gc.fillOval(gap, gap, squareSize, squareSize)
  }

  override def displayGameOver(winner: String): Unit = {
    if (this.gameOverDialog.show(winner)) {
      boardController.onDialogConfirm(this)
    }
  }

  override def clearAll(): Unit = {
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        clearSquare(i, j)
      }
    }
  }

  private def clearSquare(row: Int, col: Int): Unit = {
    val gc = grid(row)(col).getGraphicsContext2D
    gc.restore()
    gc.clearRect(0, 0, squareSize + 2 * gap, squareSize + 2 * gap)
    gc.strokeRect(0, 0, squareSize + 2 * gap, squareSize + 2 * gap)
  }
}


class DefaultBoardView(val boardController: BoardController) extends BorderPane with Listener[String] {
  private val statusLabel: Label = {
    val label = new Label("")
    label.setPadding(Insets(20))
    label.setStyle("-fx-font: 24 arial;")
    label
  }
  this.setCenter(new VBox(
    statusLabel,
    new BoardViewGrid(boardController)
  ))

  boardController.playerTurnInfoSubject.subscribe(this)

  override def notify(value: String): Unit = {
    statusLabel.text = value
  }
}
