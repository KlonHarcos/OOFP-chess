package game

import scala.util.Random

abstract class Player(val shade: Shade) {
    def getMove(board: Board): Move
}

class RandomRobot(shade: Shade) extends Player(shade: Shade) {
    def getMove(board: Board): Move = {
        val moves = board.getAllLegalMoves(shade)
        val randNum = Random.nextInt(moves.length)
        return moves(randNum)
    }
}

class RobotOnlyEval(shade: Shade) extends Player(shade: Shade) {
    def eval(board: Board): Int = {
            var whiteVal: Int = 0
            var blackVal: Int = 0
            board.getTiles().foreach{t =>
                t.piece.foreach{ p => p.shade match
                    case Dark => blackVal += p.value
                    case Light => whiteVal += p.value
                } 
            }
            shade match {
                case Dark => blackVal - whiteVal
                case Light => whiteVal - blackVal
            }
    }

    def getMove(board: Board): Move = {
        val moves = board.getAllLegalMoves(shade)
        val ratedMoves: Seq[(Move, Int)] = moves.map{ m =>
            val b = board.deepCopy()
            b.movePiece(m)
            (m, eval(b))
        }

        val sorted = ratedMoves.sortBy((m, v) => v)(Ordering[Int].reverse)
        val highestEval = sorted(0)._2
        val bestMoves = sorted.filter((_, v) => v == highestEval)
        val randNum = Random.nextInt(bestMoves.length)
        bestMoves(randNum)._1
    }
}

class DumbRobotNoPruning(shade: Shade, maxDepth: Int) extends Player(shade) { // Depth 3 makes is sooooo slow already
    def eval(board: Board): Int = {
        var whiteVal: Int = 0
            var blackVal: Int = 0
            board.getTiles().foreach{t =>
                t.piece.foreach{ p => p.shade match
                    case Dark => blackVal += p.value
                    case Light => whiteVal += p.value
                } 
            }
            whiteVal - blackVal //+ is white winning - is black winning
    }

    def evalMove(board: Board, move: Move): Int = {
        val b = board.deepCopy()
        val onTurn = b.hasTurn
        b.movePiece(move)
        onTurn match 
            case Light => eval(b)
            case Dark => -eval(b)
    }

    def getBestMovesInPostition(pos: Board): Seq[(Move, Int)] = {
        val legalMoves = pos.getAllLegalMoves(pos.hasTurn)
        if legalMoves.isEmpty then return Seq.empty[(Move, Int)]
        val evals = legalMoves.map(m => (m, evalMove(pos, m)))
        val bestMoveEval = evals.maxBy(_._2)._2
        val bestMoves = evals.filter((_, e) => e == bestMoveEval)
        return bestMoves
    }

    def searchMove(board: Board, move: Move, depth: Int = 0): Int = {
        if (depth == maxDepth) {
            return evalMove(board, move)
        } 
        else {
            val moves = board.getAllLegalMoves(board.hasTurn)
            if moves.isEmpty then return -50
            
            val results = moves.map{ m =>
                searchMove(board, m, depth + 1)
            }
            return results.max
        }
        
    }

    def getMove(board: Board): Move = {
        val moves = board.getAllLegalMoves(shade)
        val movesEvald = moves.map{ m => 
            (searchMove(board, m), m)
        }
        val max = movesEvald.maxBy(_._1)
        val topMoves = movesEvald.filter((eval, _) => eval == max._1).map((_,m) => m)
        val randNum = Random.nextInt(topMoves.length)
        return topMoves(randNum)
    }
} 