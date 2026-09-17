package game

import engine.ChessBase
import processing.core.PApplet
import engine.graphics.Rectangle
import engine.graphics.Position
import engine.graphics.{HighlightGreen, LowHighlightGreen}
import scala.collection.mutable.Map

def FenStart = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
def fenEx = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

class ChessGame extends ChessBase {
    val board = new Board(boardSize, appSize)
    val chessLogic = new ChessLogic(board, record)
    val record = GameRecord(FenStart, board)
    val player1: Player = new Player("Light")
    val player2: Player = new Player("Dark")

    var highlightedTile: Option[Tile] = None
    var validMoveHighlights: Seq[Tile] = Seq.empty[Tile]

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

    override def mousePressed(): Unit = {
        val mouse = Position(mouseX.toFloat, mouseY.toFloat)
        val currHighlight: Option[Tile] = highlightedTile
        highlightedTile = board.getTile(mouse)
        (currHighlight, highlightedTile) match {
            case (Some(from), Some(to)) if from == to => highlightedTile = None
            case (Some(from), Some(to)) => val res = board.movePiece(from, to); if res then highlightedTile = None
            case _ => ()
        }
        highlightedTile.foreach(t => t.piece.foreach(p => println(p.hasMoved)))
        validMoveHighlights = highlightedTile.match {
            case Some(t) => board.getValidMoves(t)
            case None => Seq.empty[Tile]

        }
            
        println("highlighted possible moves: " + validMoveHighlights)
        
    }


    def drawPiece() = {

    }
}


object ChessGame {
    def main(args: Array[String]): Unit = {
        PApplet.main("game.ChessGame")
    }
}

