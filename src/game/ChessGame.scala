package game

import engine.ChessBase
import processing.core.PApplet
import engine.graphics.Rectangle
import engine.graphics.Position
import engine.graphics.HighlightGreen
import scala.collection.mutable.Map

class ChessGame extends ChessBase {
    val board = Board(boardSize, appSize)
    def tiles: Map[(Char, Int), Tile] = board.tiles
    val chessLogic = new ChessLogic(tiles)

    var highlightedTile: Option[Tile] = None

    def printBoard(): Unit = {}

    override def draw(): Unit = {
        drawBoard()
        drawHighlightedTile()
    }

    override def setupGame(): Unit = {
        chessLogic.setup()
    }

    def drawBoard() = {
        background(255)
        for (tile <- tiles.values) {
            drawRectangle(tile.rect)
            tile.piece.foreach {p => 
                val sprite = pieceImgMap(p.name)
                image(sprite, tile.rect.pos.x, tile.rect.pos.y)
            }
        }

    }

    def drawHighlightedTile() = {
        highlightedTile.foreach( r => drawRectangle(r.rect.copy(color = HighlightGreen)))
    }

    override def mousePressed() = {
        val mouse = Position(mouseX.toFloat, mouseY.toFloat)
        for (tile <- tiles.values) {
            if (tile.isInside(mouse)) {
                val currHighlight: Option[Tile] = highlightedTile
                highlightedTile = Some(tile)
                (currHighlight, highlightedTile) match {
                    case (Some(from), Some(to)) if from == to => highlightedTile = None
                    case (Some(from), Some(to)) => chessLogic.movePiece(from, to); highlightedTile = None
                    case _ => ()
                }
            }
        }
        
    }


    def drawPiece() = {

    }
}


object ChessGame {
    def main(args: Array[String]): Unit = {
        PApplet.main("game.ChessGame")
    }
}

