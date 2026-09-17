package game

import engine.graphics.{Rectangle, Position, Black, White}
import scala.collection.mutable.Map

class Board(val boardSize: Int, screenDims: Int) {

    def getValidMoves(tile: Tile): Seq[Tile] = {
        println("gettingValidMove")
        tile.piece match {
            case None => Seq.empty[Tile]
            case Some(p) => p.movePattern.flatMap(f  => f(tile)(this))
        }
    }

    def placePiece(tile: Tile, p: Option[Piece]) = {
        tiles(tile.name) = tile.copy(piece = p)
    }

    def placePiece(tile: String, p: Option[Piece]) = {
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
        println(this.toString())
    }
    
    def tileSize = screenDims.toFloat / boardSize

    val tiles: Map[String, Tile] = {
        val pairs = for {
            l <- 'a' until ('a' + boardSize).toChar
            n <- 1 to boardSize
            str = l.toString + n.toString
        } yield (str, makeTile(str))
        Map.from(pairs)
    }

    def getTileRect(str: String): Option[Rectangle] = {
        Some(tiles(str).rect)
    }

    def getTile(str: String): Option[Tile] = tiles.get(str)

    def getTile(l: Char, n: Int): Option[Tile] = {
        val str = l.toString + n.toString
        getTile(str)
    }

    def getTile(mousePostion: Position): Option[Tile] = tiles.values.find(_.isInside(mousePostion))


    def makeTile(notation: String): Tile = {
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
            case "Dark" => Black
            case "Light" => White
        }
        Rectangle(pos, color, tileSize)
    }

    def notationStot(notation: String): (Char, Int) =
        (notation(0), notation(1).toString.toInt)

    def getShade(notation: String): Shade = {
        val (l, n) = notationStot(notation)
        if (l.toInt + n) % 2 == 0 then "Dark" else "Light"
    }

    def getRanks(): Seq[Seq[Tile]] = {
        Seq.from(tiles.values).groupBy(_.name(1)).values.toSeq.map(s => s.sortBy(_.name))
    }

    def getTiles(): Seq[Tile] = Seq.from(tiles.values).sortBy(_.name).toSeq

    override def toString(): String = {var strBuilder = ""
        val rankList = getRanks().sortBy(s => s(0).name).reverse
        for (rank <- rankList) {
            var emptyCounter = 0
            for (t <- rank) {
                t.piece match {
                    case None => emptyCounter += 1
                    case Some(p) if emptyCounter == 0 => strBuilder += p.name
                    case Some(p) => {
                        strBuilder += emptyCounter.toString + p.name
                        emptyCounter = 0
                    } 
                }
            }
            if emptyCounter != 0 then strBuilder += emptyCounter
            strBuilder += "/"
        }
        strBuilder = strBuilder.dropRight(1)
        strBuilder
    }
}
