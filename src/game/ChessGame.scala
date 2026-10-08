package game

import engine.ChessBase
import processing.core.PApplet
import engine.graphics.Rectangle
import engine.graphics.Position
import engine.graphics.{HighlightGreen, LowHighlightGreen}
import game.{Shade, Light, Dark}
import scala.collection.mutable.Map


class ChessGame extends ChessBase {
    val board = Board(boardSize, appSize)
    ////val player1: Player = new Player(Light)
    //val player2: Player = new SmarterRobotNoPruning(Dark, 3)

    var highlightedTile: Option[Tile] = None
    var validMoveHighlights: Seq[Tile] = Seq.empty[Tile]
    var debugHighlights: Seq[Tile] = Seq.empty[Tile]

    override def draw(): Unit = {
        drawBoard()
        drawHighlightedTile()
        drawValidMoves()
        //if player2.shade == board.hasTurn then board.tryMove(player2.getMove(board))
    }

    def drawBoard() = {
        background(255)
        for (tile <- board.getTiles()) {
            drawRectangle(tile.rect)
            tile.piece.foreach {p => 
                val sprite = pieceImgMap(p.name)
                image(sprite, tile.rect.pos.x, tile.rect.pos.y)
            }
        }

    }

    def drawHighlightedTile() = highlightedTile.foreach( r => drawRectangle(r.rect.copy(color = HighlightGreen)))
    def drawValidMoves() = validMoveHighlights.foreach(r => drawRectangle(r.rect.copy(color = LowHighlightGreen)))
    def drawDebug() = debugHighlights.foreach(r => drawRectangle(r.rect.copy(color = LowHighlightGreen)))
    def clearHighlights(): Unit = {
        highlightedTile = None
        validMoveHighlights = Seq.empty[Tile]
    }
    override def mousePressed(): Unit = {
        val mouse = Position(mouseX.toFloat, mouseY.toFloat)
        val currHighlight: Option[Tile] = highlightedTile 
        (currHighlight, board.getTile(mouse)) match {
            case (Some(from), Some(to)) if from == to => clearHighlights()
            case (Some(from), Some(to)) => clearHighlights(); board.movePiece(from, to); board.refreshVisuals()
            case (_, Some(to)) => 
                highlightedTile = Some(to)
                validMoveHighlights = board.getMoves(to.piece)
            case _ => ()
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

