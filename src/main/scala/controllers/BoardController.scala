package com.olibipin
package controllers

import views.BoardView
import engine.Game
import observable.Subject

import scalafx.application.Platform

import scala.concurrent.{Future}
import scala.concurrent.ExecutionContext.Implicits.global

trait Controller {
  def handlePlayAt(row: Int, col: Int, board: BoardView): Unit
  def onDialogConfirm(board: BoardView): Unit
}

class BoardController extends Controller {
  val gameEngine = new Game()
  val playerTurnInfoSubject: Subject[String] = new Subject[String] {}

  informAboutNewPlayer()

  override def handlePlayAt(row: Int, col: Int, board: BoardView): Unit = {
    if (gameEngine.isFree(row, col)) {
      paintSquare(row, col, board)
      gameEngine.playAt(row, col)
      informAboutNewPlayer()
      if (!checkForGameOver(board)) {
        for ( bestMove <- findBestMoveAI()) {
          val (bestMoveR, bestMoveC) = bestMove
          // this code block is a callback
          // it will run in a separate thread
          // however the UI updates must run on the original JavaFX thread
          // for that we need to use Platform.runLater()
          // it makes the following block run back in the UI thread
          val runnable = new Runnable {
            override def run(): Unit = {
              paintSquare(bestMoveR, bestMoveC, board)
              gameEngine.playAt(bestMoveR, bestMoveC)
              informAboutNewPlayer()
              checkForGameOver(board)
            }
          }
          Platform.runLater(runnable)
        }
      }
    }
  }

  private def checkForGameOver(board: BoardView): Boolean = {
    val (hasWinner, winnerName) = gameEngine.hasAWinner
    if (hasWinner) {
      board.displayGameOver(winner = winnerName)
      return true
    }
    false
  }

  private def findBestMoveAI(): Future[(Int, Int)] = {
    // waiting some time for theatrics
    Future {
      Thread.sleep(1_000)
      gameEngine.findBestMove()
    }
  }

  override def onDialogConfirm(board: BoardView): Unit = {
    gameEngine.resetGame
    board.clearAll()
    informAboutNewPlayer()
  }

  private def informAboutNewPlayer(): Unit = {
    if (gameEngine.isFirstPlayerNext) {
      playerTurnInfoSubject.update("Next player: X")
    } else {
      playerTurnInfoSubject.update("Next player: O")
    }
  }

  private def paintSquare(row: Int, col: Int, board: BoardView): Unit = {
    if (gameEngine.isFirstPlayerNext) {
      board.paintCross(row, col)
    } else {
      board.paintCircle(row, col)
    }
  }
}


