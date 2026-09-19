package game

import engine.ChessBase
import processing.core.PApplet
import engine.graphics.Rectangle
import engine.graphics.Position
import engine.graphics.{HighlightGreen, LowHighlightGreen}
import game.{Shade, Light, Dark}
import scala.collection.mutable.Map

def FenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
def fenEx = "rnbqkbnr/pp2pppp/3p4/2pP4/8/8/PPP1PPPP/RNBQKBNR w KQkq c6 0 3"

class ChessGame extends ChessBase {
    val board = Board(boardSize, appSize)
    val chessLogic = new ChessLogic(board, record)
    val record = GameRecord(fenEx, board)
    val player1: Player = new Player(Light)
    val player2: Player = new Player(Dark)

    var highlightedTile: Option[Tile] = None
    var validMoveHighlights: Seq[Tile] = Seq.empty[Tile]
    var debugHighlights: Seq[Tile] = Seq.empty[Tile]

    override def draw(): Unit = {
        drawBoard()
        drawHighlightedTile()
        drawValidMoves()
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

    override def mousePressed(): Unit = {
        val mouse = Position(mouseX.toFloat, mouseY.toFloat)
        val currHighlight: Option[Tile] = highlightedTile
        highlightedTile = board.getTile(mouse)
        (currHighlight, highlightedTile) match {
            case (Some(from), Some(to)) if from == to => highlightedTile = None
            case (Some(from), Some(to)) => val res = board.tryMove(from, to); if res then highlightedTile = None; val state = board.gameState(); if state != "running" then println(state)
            case _ => ()
        }
        validMoveHighlights = Seq.empty[Tile] 
        highlightedTile.foreach(t => 
            t.piece.foreach(p => 
                if p.shade == board.hasTurn then 
                    validMoveHighlights = board.getPossibleTargetTile(t)))
        debugHighlights = Seq.empty[Tile]
        debugHighlights = board.getVision(Light)
    }


    def drawPiece() = {

    }
}


object ChessGame {
    def main(args: Array[String]): Unit = {
        PApplet.main("game.ChessGame")
    }
}

