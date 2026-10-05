package game

import engine.graphics.{Rectangle, Position, Black, White}
import scala.collection.mutable.Map as MMap

class Board(tiles: MMap[Notation, Tile]) {
    var state: GameState
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
}