package game

import engine.graphics.{Rectangle, Position, Black, White}
import scala.collection.mutable.Map as MMap

class Board(tiles: MMap[Notation, Tile]) {
    var state: GameState = GameState(Board.FenEx)
    refreshVisuals()
    
    def refreshVisuals(): Unit = {
        Notation.all.foreach(n => placePiece(n, None))
        val pieceMap = state.piecePositions
        pieceMap.keys.foreach(p => placePiece(pieceMap(p), Some(p)))
    }

    def placePiece(tile: Tile, p: Option[Piece]) = {
        tiles(tile.name) = tile.copy(piece = p)
    }

    def placePiece(tile: Notation, p: Option[Piece]) = {
        tiles(tile) = tiles(tile).copy(piece = p)
    }

    def getMoves(piece: Option[Piece]): Seq[Tile] = state.getPseudoLegalMoves(piece).map(tiles(_)).toSeq

    def movePiece(from: Tile, to: Tile): Unit = {
        println("moving piece")
        state = state.tryMove(from.name, to.name)
    } 

    def getTiles(): Seq[Tile] = tiles.values.toSeq

    def getTile(mousePostion: Position): Option[Tile] = tiles.values.find(_.isInside(mousePostion))
}

object Board {
    def apply(boardSize: Int, screenDims: Int): Board = {
        def makeTile(notation: Notation): Tile = {
            val r = makeTileRect(notation)
            val s = getShade(notation)
            Tile(s, r, notation)
        }
        def makeTileRect(notation: String): Rectangle = {
            val (l, n) = notationStot(notation)
            val x = ((l - 'a') * tileSize)
            val y = (boardSize - (n)) * tileSize
            val pos = Position(x, y)
            val color = getShade(notation) match {
                case Dark => Black
                case Light => White
            }
            Rectangle(pos, color, tileSize)
        }
        def tileSize = screenDims.toFloat / boardSize
        val tiles: MMap[Notation, Tile] = {
            val pairs = for {
                l <- 'a' until ('a' + boardSize).toChar
                n <- 1 to boardSize
                str = l.toString + n.toString
            } yield (str, makeTile(str))
            MMap.from(pairs)
        }
        new Board(tiles)
    }

    def getShade(notation: String): Shade = {
        val (l, n) = notationStot(notation)
        if (l.toInt + n) % 2 == 0 then Dark else Light
    }

    def notationStot(notation: String): (Char, Int) = (notation(0), notation(1).asDigit)  
    val FenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    val FenEx = "8/6p1/pn2kp1p/1p5P/1B3PP1/1P6/P4K2/8 w - - 2 36"

}