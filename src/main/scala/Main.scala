package com.olibipin

import scala.io.StdIn

import game._

object Main {
    def main(args: Array[String]): Unit = {
        val nextPlayer = StdIn.readLine.trim().charAt(0)
        val isFirstPlayer = nextPlayer == 'X'
        val lines = Array(
            StdIn.readLine.trim(),
            StdIn.readLine.trim(),
            StdIn.readLine.trim(),
        )
        val game = new Game()
        for (i <- 0 to 2) {
            for (j <- 0 to 2) {
                game.gameState(i)(j) = {
                    lines(i)(j) match {
                        case '_' => Node.empty
                        case 'X' => Node.first
                        case _ => Node.second
                    }
                }
            }
        }

        val bestMove = game.findBestMove(isFirstPlayer = isFirstPlayer)
        for (i <- 0 to 2) {
            for (j <- 0 to 2) {
                if (bestMove(i)(j) != game.gameState(i)(j)) {
                    println(s"${i} ${j}")
                }
            }
        }
    }
}


