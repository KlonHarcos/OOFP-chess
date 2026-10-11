package game

import scala.util.Random
import scala.math.{max, min}

abstract class Player(val shade: Shade) {
    def getMove(state: GameState): Move
}

class RandomBot(shade: Shade) extends Player(shade: Shade) {
    def getMove(state: GameState): Move = {
        getRandomMove(state.allLegalMoves)
    }

    def getRandomMove(moves: Set[Move]): Move = {
        val seq = moves.toSeq
        val randNum = Random.nextInt(seq.length)
        val move = seq(randNum)
        println("returning move " + move)
        move
    }
}

class StaticEvalBot(shade: Shade) extends RandomBot(shade: Shade) {
    override def getMove(state: GameState): Move = {
        val moveEvals = evaluateAllMoves(state)
        val best = getBestMoves(moveEvals)
        println("giving random move")
        getRandomMove(best)
    }

    def evaluateAllMoves(state: GameState): Set[(Int,Move)] = {
        val moves = state.allLegalMoves
        moves.map{m =>
            val newState = state.tryMove(m.from, m.to)
            (StaticEval.evaluate(newState), m)
        }
    }

    def getBestMoves(evals: Set[(Int, Move)]): Set[Move] = {
        val best = shade match {
            case Dark => evals.map(_._1).min
            case Light => evals.map(_._1).max
        }
        val top = evals.filter(_._1 == best)
        top.map(_._2)
    }
}

class MiniMaxBot(shade: Shade) extends StaticEvalBot(shade: Shade) {

    val MaxDepth = 5
    override def getMove(state: GameState): Move = {
        val evals = evalMovesMiniMax(state)
        val bests = getBestMoves(evals)
        getRandomMove(bests)
    }

    def evalMovesMiniMax(state: GameState): Set[(Int, Move)] = {
        val moves = state.allLegalMoves
        moves.map{move =>
            val newState = state.tryMove(move.from, move.to)
            (miniMaxEval(newState, MaxDepth), move)
        }
    }

    def miniMaxEval(state: GameState, depth: Int): Int = {
        if depth == 0 || state.isGameOver then return StaticEval.evaluate(state)

        val (worstVal, comparisonFunc) = state.hasTurn match {
            case Dark =>  (Int.MinValue, min)
            case Light => (Int.MaxValue, max)
        }

        var res = worstVal
        state.allLegalMoves.foreach{move =>
            val newState = state.tryMove(move.from, move.to)
            val eval = miniMaxEval(newState, depth - 1)
            res = comparisonFunc(res, eval)
        }
        res

    }
}

object StaticEval {
    def evaluate(state: GameState): Int = {
        val (whitePieces, blackPieces) = state.allPieces.partition(p => p.shade == Light)
        val whiteVal = whitePieces.map(_.value).sum
        val blackVal = blackPieces.map(_.value).sum
        whiteVal - blackVal
    }
}