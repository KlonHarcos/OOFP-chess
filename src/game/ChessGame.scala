package game

import engine.ChessBase
import processing.core.PApplet
import engine.graphics.Rectangle
import engine.graphics.Position
import engine.graphics.{HighlightGreen, LowHighlightGreen, Red}
import game.{Shade, Light, Dark}
import scala.collection.mutable.Map


class ChessGame extends ChessBase {
    val board = Board(boardSize, appSize)
    //val player2: OptionPlayer = Some(new AlphaBetaBot(Dark))
    val player2 = Some(new StockFish(Dark))
    //val player2: Option[Player] = None

    var highlightedTile: Option[Tile] = None
    var validMoveHighlights: Seq[Tile] = Seq.empty[Tile]
    var lastClick: Long = 0
    val MaxWait = 500
    

    override def draw(): Unit = {
        drawBoard()
        drawHighlightedTile()
        drawValidMoves()
        drawGameOverScreen()
        tryP2Move()
    }

    def drawGameOverScreen(): Unit = {
        board.getGameEndMessage match {
            case None => ()
            case Some(value) =>
                fill(Red)
                drawTextCentered(value, 50, Position(appSize.toFloat/2, appSize.toFloat/2))
        }
        
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
    def clearHighlights(): Unit = {
        highlightedTile = None
        validMoveHighlights = Seq.empty[Tile]
    }
    override def mousePressed(): Unit = {
        val mouse = Position(mouseX.toFloat, mouseY.toFloat)
        val currHighlight: Option[Tile] = highlightedTile 
        (currHighlight, board.getTile(mouse)) match {
            case (Some(from), Some(to)) if from == to => clearHighlights()
            case (Some(from), Some(to)) => clearHighlights(); board.movePiece(from, to); board.refreshVisuals(); drawBoard()
            case (_, Some(to)) => 
                highlightedTile = Some(to)
                validMoveHighlights = board.getMoves(to.piece)
            case _ => ()
        }

        lastClick = System.currentTimeMillis()
    }

    def tryP2Move(): Unit = {
        if (System.currentTimeMillis() - lastClick) < MaxWait then return
        for (p2 <- player2) {
            if (p2.shade == board.state.hasTurn) {
                val move = p2.getMove(board.state)
                if (move != Move.empty) {
                    board.movePiece(move)
                    board.refreshVisuals()
                }
            }
        }
        
    }
}


object ChessGame {
    def main(args: Array[String]): Unit = {
        PApplet.main("game.ChessGame")
    }
}

