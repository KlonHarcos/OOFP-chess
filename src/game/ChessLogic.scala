package game

import scala.collection.mutable.Map

class ChessLogic(tiles: Map[Notation, Tile]) {
    val fenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    def setup(): Unit = {
        loadFENnotation(fenStart)
    }

    def placePiece(tile: Tile, p: Option[Piece]) = {
        tiles(tile.nameAsNotation()) = tile.copy(piece = p)
    }

    def placePiece(tile: Notation, p: Option[Piece]) = {
        tiles(tile) = tiles(tile).copy(piece = p)
    }

    def movePiece(from: Tile, to: Tile): Unit = {
        (from.piece, to.piece) match {
            case (Some(atck), Some(prot)) if atck.shade == prot.shade => return
            case (Some(_), None) |
                 (Some(_), Some(_)) =>
                placePiece(to, from.piece)
                placePiece(from, None)
            case _ => return
        }
    }
    def loadFENnotation(fen: String) = {
        val splitFen = fen.split(" ")
        val boardRows = splitFen(0).split("/")
        val nextToMove = splitFen(1)
        val castlingRights = splitFen(2)
        val enPassantSquare = splitFen(3)
        val halfMoveClock = splitFen(4)
        val fullMoveClock = splitFen(5)

        for ((rowStr, index) <- boardRows.reverse.zipWithIndex) {
            val rowLetter: Char = ('a' + index).toChar
            var colNum = 1
            for (c <- rowStr) {
                if (c.isDigit) colNum += c.toString.toInt
                else {
                    val p: Piece = Piece.getPiecebyName(c)
                    placePiece((rowLetter, colNum), Some(p))
                    colNum += 1
                }
            }
        }
    }



}