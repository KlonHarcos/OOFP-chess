package game

import engine.graphics.{Rectangle, Position, Black, White}
import scala.collection.mutable.Map

//case class Notation(letter: Char, number: Int)
type Notation = (Char, Int)
case class Board(boardSize: Int, screenDims: Int) {
    
    def tileSize = screenDims.toFloat / boardSize

    val tiles: Map[Notation, Tile] = {
        val pairs = for {
            l <- 'a' until ('a' + boardSize).toChar
            n <- 1 to boardSize
            notation = (l,n)
        } yield (notation, makeTile(notation))

        Map.from(pairs)
    }

    def getRect(str: String): Option[Rectangle] = {
        val n = (str(0), str(1).toString.toInt)
        Some(tiles(n).rect)
    }

    def makeTile(notation: Notation): Tile = {
        val (l,n) = notation
        val r = getTileRect(l,n)
        val s = getShade(l,n)
        val name = l.toString + n.toString
        Tile(s, r, name)
    }

    def getTileRect(notation: Notation): Rectangle = {
        val (l, n) = notation
        val x = ((n - 1) * tileSize)
        val y = (boardSize - (l - 'a' + 1)) * tileSize
        val pos = Position(x, y)
        val color = getShade(l, n) match {
            case "Dark" => Black
            case "Light" => White
        }
        Rectangle(pos, color, tileSize)
    }

    def getShade(l: Char, n: Int): Shade = if (l.toInt + n) % 2 == 0 then "Dark" else "Light"
}
