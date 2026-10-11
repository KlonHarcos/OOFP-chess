package game

import scala.util.Random
import scala.util.boundary, boundary.break
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
        move
    }
}

class StaticEvalBot(shade: Shade) extends RandomBot(shade: Shade) {
    override def getMove(state: GameState): Move = {
        val moveEvals = evaluateAllMoves(state)
        //moveEvals.foreach(println)
        val best = getBestMoves(moveEvals)
        getRandomMove(best)
    }

    def evaluateAllMoves(state: GameState): Set[(Double, Move)] = {
        val moves = state.allLegalMoves
        //println("all legal moves: ")
        //moves.foreach(println)
        moves.map{m =>
            val newState = state.tryMoveUnsafe(m.from, m.to)
            (StaticEval.evaluate(newState), m)
        }
    }

    def getBestMoves(evals: Set[(Double, Move)]): Set[Move] = {
        val best = shade match {
            case Dark => evals.map(_._1).min
            case Light => evals.map(_._1).max
        }
        val top = evals.filter(_._1 == best)
        top.map(_._2)
    }
}

class MiniMaxBot(shade: Shade) extends StaticEvalBot(shade: Shade) {

    val MaxDepth = 3
    override def getMove(state: GameState): Move = {
        val evals = evalMovesMiniMax(state)
        evals.foreach(println)
        val bests = getBestMoves(evals)
        getRandomMove(bests)
    }

    def evalMovesMiniMax(state: GameState): Set[(Double, Move)] = {
        val moves = state.allLegalMoves
        //println(moves)
        moves.map{move =>
            val newState = state.tryMove(move.from, move.to)
            (miniMaxEval(newState, MaxDepth), move)
        }
    }

    def miniMaxEval(state: GameState, depth: Int): Double = {
        if (depth == 0 || state.isGameOver) {
            return StaticEval.evaluate(state)
        }

        val (worstVal, comparisonFunc) = state.hasTurn match {
            case Dark =>  (Double.PositiveInfinity, (x: Double, y: Double) => min(x,y))
            case Light => (Double.NegativeInfinity, (x: Double, y: Double) => max(x,y))
        }

        var res = worstVal
        state.allPseudoLegalMoves.foreach{move =>
            val newState = state.tryMoveUnsafe(move.from, move.to)
            val eval = miniMaxEval(newState, depth - 1)
            res = comparisonFunc(res, eval)
        }
        res

    }
}

class AlphaBetaBot(shade: Shade) extends MiniMaxBot(shade: Shade) {
    override val MaxDepth: Int = 5
    override def getMove(state: GameState): Move = {
        val evals = evalMovesAlphaBeta(state)
        evals.foreach(println)
        val bests = getBestMoves(evals)
        getRandomMove(bests)
    }

    def evalMovesAlphaBeta(state: GameState): Set[(Double, Move)] = {
        val moves = state.allLegalMoves
        //println(moves)
        //val isMaxing = state.hasTurn.other == Light
        moves.map{move =>
            val newState = state.tryMove(move.from, move.to)
            (AlphaBetaEval(newState, MaxDepth, Double.NegativeInfinity, Double.PositiveInfinity), move)
        }
    }

    def AlphaBetaEval(state: GameState, depth: Int, alpha: Double, beta: Double): Double = {
        if (depth == 0 || state.isGameOver) {
            return StaticEval.evaluate(state)
        }

        var a = alpha
        var b = beta
        if (state.hasTurn == Light) {
            var maxEval = Double.NegativeInfinity
            boundary {
            for(move <- state.allPseudoLegalMoves) {
                val newState = state.tryMoveUnsafe(move.from, move.to)
                val eval = AlphaBetaEval(newState, depth - 1, a, b)
                maxEval = max(maxEval, eval)
                a = max(a, eval)
                if b <= a then break()
            }}
            return maxEval
        } else {
            var minEval = Double.PositiveInfinity
            boundary {
            for(move <- state.allPseudoLegalMoves) {
                val newState = state.tryMoveUnsafe(move.from, move.to)
                val eval = AlphaBetaEval(newState, depth - 1, a, b)
                minEval = min(minEval, eval)
                b = min(b, eval)
                if b <= a then break()
            }}
            return minEval
        }
    }
}

object StaticEval {
    def evaluate(state: GameState): Double = {
        if state.isStaleMate then return 0.0
        if (state.isCheckMate) {
            return state.hasTurn match {
                case Dark => 1000
                case Light => -1000
            }
        }
        
        val (whitePieces, blackPieces) = state.allPieces.partition(p => p.shade == Light)
        val whiteVal = whitePieces.toSeq.map(_.value).sum //toSeq is a crazy bug
        val blackVal = blackPieces.toSeq.map(_.value).sum
        val check = if state.isInCheck(Light) then -1
            else if state.isInCheck(Dark) then 1 else 0
        whiteVal - blackVal + check
    }
    val PositionConst = 1

    def getPositionScore(p: Piece, pos: Notation) = p match {
        case p @Pawn(shade, id) => {
            p.promotionRank.toDouble - pos.rank / 7
        }
        case Knight(shade, id) => 
        case Bishop(shade, id) => 
        case Rook(shade, id) => 
        case Queen(shade, id) => 
        case King(shade, id) =>
    }
}