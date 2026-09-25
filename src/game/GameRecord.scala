package game

import scala.compiletime.ops.boolean
import dotty.tools.dotc.core.StdNames.str

case class GameRecord(  board: Board,
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
        board.castlingRights = board.castlingRights.setAll(false)
        castlingRights.foreach(c =>
            val (shade, side) =  c match {
                case 'k' | 'K' => val s = if c.isUpper then Light else Dark; (s, KingSide)
                case 'q' | 'Q' => val s = if c.isUpper then Light else Dark; (s, QueenSide)
            }
            board.castlingRights = board.castlingRights.set(shade, side, true)
        )
        val enPassantSquare = splitFen(3)
        board.enPassantMove = if enPassantSquare == "-" then None else 
            val takeTile = board.getTileUnsafe(enPassantSquare)
            val removeTileRank = if takeTile.rank == 6 then 5 else 4
            Some(enPassant(takeTile, board.getTileUnsafe(takeTile.file.toString + removeTileRank.toString)))
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
                    if (p.isInstanceOf[Pawn]) {
                        val startingRank = p.shade match {
                            case Dark => 7
                            case Light => 2
                        }
                        if (rowNum != startingRank) board.placePiece(strTileName, Some(p.move()))
                    }
                    
                    colNum += 1
                }
            }
        }

        GameRecord(board, board.getTile(enPassantSquare), halfMoveClock.toInt, fullMoveClock.toInt)
    }

    
}