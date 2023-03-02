package com.olibipin
package engine

import scala.math._
import scala.util.Random

object Node extends Enumeration {
  val empty = Value("_")
  val first = Value("X")
  val second = Value("0")
}

class Game {
  // X - first player (maximizer)
  // 0 - second player (minimizer)
  type GameState = Array[Array[Node.Value]]
  private var nextToGoIsFirstPlayer: Boolean = new Random().nextBoolean()

  private var gameState = Array(
    Array(Node.empty, Node.empty, Node.empty),
    Array(Node.empty, Node.empty, Node.empty),
    Array(Node.empty, Node.empty, Node.empty)
  )

  def hasAWinner: (Boolean, String) = {
    evaluatePosition(gameState) match {
      case 0 => (false, "")
      case Int.MaxValue => (true, Node.first.toString)
      case _ => (true, Node.second.toString)
    }
  }

  def resetGame: Unit = {
    nextToGoIsFirstPlayer = new Random().nextBoolean()
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        gameState(i)(j) = Node.empty
      }
    }
  }

  def isFirstPlayerNext: Boolean = nextToGoIsFirstPlayer

  def isFree(row: Int, col: Int): Boolean = gameState(row)(col) == Node.empty

  def playAt(row: Int, col: Int): Unit = {
    if (gameState(row)(col) != Node.empty) throw new Exception("the position is already occupied by a piece")
    gameState(row)(col) = if (nextToGoIsFirstPlayer) Node.first else Node.second
    nextToGoIsFirstPlayer = !nextToGoIsFirstPlayer
  }

  def findBestMove(): (Int, Int) = {
    val bestMoveState = findBestMove(nextToGoIsFirstPlayer)
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        if (bestMoveState(i)(j) != gameState(i)(j))
          return (i, j)
      }
    }
    throw new Exception("couldn't find a move")
  }

  private def findBestMove(isFirstPlayer: Boolean): GameState = {
    val (score, bestMove) = alphaBeta(gameState, Int.MinValue, Int.MaxValue, 100, isFirstPlayer)
    bestMove
  }

  private def alphaBeta(state: GameState, givenAlpha: Int, givenBeta: Int, depth: Int, isMaximizing: Boolean): (Int, GameState) = {
    var alpha = givenAlpha
    var beta = givenBeta

    if (depth == 0 || findNextPossibleMoves(state, isFirst = true).size == 0) {
      return (evaluatePosition(state), state)
    }

    if (isMaximizing) {
      var value = Int.MinValue
      var bestMove: GameState = null
      findNextPossibleMoves(state, isFirst = true).foreach(move => {
        val (score, _) = alphaBeta(move, alpha, beta, depth-1, false)
        if (score > value) {
          value = score
          bestMove = move
        }
        if (value > beta) {
          return (value, bestMove)
        }
        alpha = max(alpha, value)
      })
      return (value, bestMove)
    }
    var value = Int.MaxValue
    var bestMove: GameState = null
    findNextPossibleMoves(state, isFirst = false).foreach(move => {
      val (score, _) = alphaBeta(move, alpha, beta, depth-1, true)
      if (score < value) {
        value = score
        bestMove = move
      }
      if (value < alpha) {
        return (value, bestMove)
      }
      beta = min(beta, value)
    })
    return (value, bestMove)
  }

  private def evaluatePosition(state: GameState): Int = {
    // first winning -> inf
    // first losing -> -inf
    def valByWinner(winner: Node.Value) = {
      winner match {
        case Node.first => Int.MaxValue
        case Node.second => Int.MinValue
        case _ => throw new Exception("empty postion can't be a winner")
      }
    }
    for (i <- 0 to 2) {
      if(state(i)(0) != Node.empty && state(i)(0) == state(i)(1) && state(i)(0) == state(i)(2))
        return valByWinner(state(i)(0))
      if(state(0)(i) != Node.empty && state(0)(i) == state(1)(i) && state(0)(i) == state(2)(i))
        return valByWinner(state(0)(i))
      if(state(0)(0) != Node.empty && state(0)(0) == state(1)(1) && state(0)(0) == state(2)(2))
        return valByWinner(state(0)(0))
      if(state(2)(0) != Node.empty && state(2)(0) == state(1)(1) && state(2)(0) == state(0)(2))
        return valByWinner(state(2)(0))
    }
    return 0
  }

  private def findNextPossibleMoves(state: GameState, isFirst: Boolean):Array[GameState] = {
    var moves = Array[GameState]()
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        if (state(i)(j) == Node.empty) {
          moves = moves :+ playAt(i, j, state, isFirst)
        }
      }
    }
    moves
  }

  private def copyState(state: GameState): GameState = {
    var retval = Array(
      Array(Node.empty, Node.empty, Node.empty),
      Array(Node.empty, Node.empty, Node.empty),
      Array(Node.empty, Node.empty, Node.empty)
    )
    for (i <- 0 to 2) {
      for (j <- 0 to 2) {
        retval(i)(j) = state(i)(j)
      }
    }
    retval
  }

  private def playAt(row: Int, col: Int, state: GameState, isFirst: Boolean): GameState = {
    var retval = copyState(state)
    retval(row)(col) = if (isFirst) Node.first else Node.second
    retval
  }

  private def gameStateToString(state: GameState) = {
    var retval = ""
    state.foreach(row => {
      row.foreach(col => retval += s"${col} ")
      retval += "\n"
    })
    retval
  }

  override def toString = {
    gameStateToString(gameState)
  }
}