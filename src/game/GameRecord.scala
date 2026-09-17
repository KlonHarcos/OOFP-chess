package game

import scala.compiletime.ops.boolean
import dotty.tools.dotc.core.StdNames.str

case class GameRecord(  board: Board,
                        nextToMove: String,
                        castlingRights: String,
                        enPassantSquare: Option[Tile],
                        halfMoveClock: Int,
                        fullMoveClock: Int
) {

    var moves: List[String] = List[String]()

    def castleBlackQueen(): Boolean = castlingRights.contains('q')
    def castleBlackKing(): Boolean = castlingRights.contains('k')
    def castleWhiteQueen(): Boolean = castlingRights.contains('K')
    def castleWhiteKing(): Boolean = castlingRights.contains('Q')
    override def toString(): String = {
        val enPass: String = enPassantSquare match {
            case None => "-"
            case Some(value) => value.name
        }
        Seq[String](board.toString, nextToMove, castlingRights, enPass, halfMoveClock.toString, fullMoveClock.toString).mkString(" ")
    }
     
}


object GameRecord {
    def apply(fen: String, board: Board): GameRecord = {
        val splitFen = fen.split(" ")
        val boardRows = splitFen(0).split("/")
        val nextToMove = splitFen(1)
        val castlingRights = splitFen(2)
        val enPassantSquare = splitFen(3)
        val halfMoveClock = splitFen(4)
        val fullMoveClock = splitFen(5)

        for ((rowStr, index) <- boardRows.reverse.zipWithIndex) {
            val rowNum: Int = index + 1
            var colNum: Int = 0
            def colLetter: Char = ('a' + colNum).toChar
            for (c <- rowStr) {
                if (c.isDigit) colNum += c.toString.toInt
                else {
                    val p: Piece = Piece.getPiecebyName(c)
                    val strTileName = colLetter.toString + rowNum
                    board.placePiece(strTileName, Some(p))
                    colNum += 1
                }
            }
        }

        GameRecord(board, nextToMove, castlingRights, board.getTile(enPassantSquare), halfMoveClock.toInt, fullMoveClock.toInt)
    }

    
}