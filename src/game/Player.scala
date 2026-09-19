package game

import scala.util.Random

class Player(val shade: Shade) {
    //def getMove(board: Board): Move
}

class RandomRobot(shade: Shade) extends Player(shade: Shade) {
    def getMove(board: Board): Move = {
        val moves = board.getAllLegalMoves(shade)
        val randNum = Random.nextInt(moves.length)
        return moves(randNum)
    }
}