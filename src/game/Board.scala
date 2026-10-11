package game

import engine.graphics.{Rectangle, Position, Black, White}
import scala.collection.mutable.Map as MMap

class Board(tiles: MMap[Notation, Tile]) {
    var state: GameState = GameState(Board.FenPromotion)
    refreshVisuals()
    
    def refreshVisuals(): Unit = {
        Notation.all.foreach(n => placePiece(n, None))
        val pieceMap = state.piecePositions
        pieceMap.keys.foreach(p => placePiece(pieceMap(p), Some(p)))
    }

    def getGameEndMessage: Option[String] = state.getWinner() match {
            case None => None
            case Some(either) => either match {
                case Right(value) => Some(f"$value won by Checkmate!")
                case Left(value) => Some(f"Its draw by $value")
            }
        }
    

    def placePiece(tile: Tile, p: Option[Piece]) = {
        tiles(tile.name) = tile.copy(piece = p)
    }

    def placePiece(tile: Notation, p: Option[Piece]) = {
        tiles(tile) = tiles(tile).copy(piece = p)
    }

    def getMoves(piece: Option[Piece]): Seq[Tile] = state.getLegalTargetTiles(piece).map(tiles(_)).toSeq

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
    val FenEnPas = "rnbqkbnr/p2ppp2/2p3pp/Pp6/8/3PB3/1PP1PPPP/RN1QKBNR w KQkq b6 0 5"
    val FenEnPasInternal = "rnbqkbnr/pp1ppp2/2p3pp/P7/8/3PB3/1PP1PPPP/RN1QKBNR b KQkq - 1 4"
    val FenCastles = "r1bqk2r/pp1p2pp/2n1p2n/2p1bp2/1P6/P1N1P1P1/QBPPNPBP/R3K2R w KQq - 5 10"
    val FenMoveCount = "7k/4B3/p5b1/8/8/8/8/7K w - - 98 100"
    val FenM1 = "rnbqkbnr/ppppp2p/8/5pp1/4P3/2P5/PP1P1PPP/RNBQKBNR w KQkq g6 0 3"
    val FenStaleMate = "k7/8/1Q6/8/8/8/8/7K b - - 0 1"
    val FenPromotion = "7k/R1P5/8/8/8/8/8/K7 w - - 0 1"
}