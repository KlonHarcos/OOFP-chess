package game

import scala.compiletime.ops.boolean
import dotty.tools.dotc.core.StdNames.str

case class GameRecord(  board: Board,
                        castlingRights: String,
                        enPassantSquare: Option[Tile],
                        halfMoveClock: Int,
                        fullMoveClock: Int
) {

    var moves: List[String] = List[String]()

    override def toString(): String = {
        val enPass: String = enPassantSquare match {
            case None => "-"
            case Some(value) => value.name
        }
        val nextToMove = board.hasTurn match {
            case Light => "w"
            case Dark => "b"
        }

        Seq[String](board.toString, nextToMove, board.castlingRights.toString, enPass, halfMoveClock.toString, fullMoveClock.toString).mkString(" ")
    }
     
}


object GameRecord {
    def apply(fen: String, board: Board): GameRecord = {
        val splitFen = fen.split(" ")
        val boardRows = splitFen(0).split("/")
        val nextToMove = splitFen(1)
        board.hasTurn = nextToMove match {
            case "w" => Light
            case "b" => Dark
        }
        val castlingRights = splitFen(2)
        val enPassantSquare = splitFen(3)
        val halfMoveClock = splitFen(4)
        val fullMoveClock = splitFen(5)

        for ((rowStr, index) <- boardRows.reverse.zipWithIndex) {
            val rowNum: Int = index + 1
            var colNum: Int = 0
            def colLetter: Char = ('a' + colNum).toChar
            for (c <- rowStr) {
                if (c.isDigit) colNum += c.asDigit
                else {
                    val p: Piece = Piece.getPiecebyName(c)
                    val strTileName = colLetter.toString + rowNum
                    board.placePiece(strTileName, Some(p))
                    colNum += 1
                }
            }
        }

        GameRecord(board, castlingRights, board.getTile(enPassantSquare), halfMoveClock.toInt, fullMoveClock.toInt)
    }

    
}