package game

import engine.graphics.{Color, Black, White, HighlightGreen}
import engine.graphics.Green
import engine.graphics.Position
import engine.graphics.Rectangle

type Shade = "Dark" | "Light"

case class Tile(shade: Shade, rect: Rectangle, name: String, piece: Option[Piece] = None) {
    def rank: Int = name(1).asDigit
    def file: Char = name(0)


    def isInside(pos: Position): Boolean = {
        if (pos.x < rect.pos.x || pos.y < rect.pos.y) return false
        if (pos.x > rect.pos.x + rect.size || pos.y > rect.pos.y + rect.size) return false
        true
    }

    override def toString(): String = name

    def row(range: Int = -1)(board: Board): Seq[Tile] = {
        val rightRes = getTilesinDir(East, board, range)
        val leftRes = getTilesinDir(West, board, range)
        rightRes ++ leftRes
    }

    def forward(shade: Shade, range: Int = -1)(board: Board): Seq[Tile] = {
        def pawnMove(dir1: Direction, dir2: Direction, dir3: Direction) = {
            val forward = getTilesinDir(dir1, board, range).filter(t => !t.piece.isDefined)
            val captures = (getTilesinDir(dir2, board, 1) ++ 
                            getTilesinDir(dir3, board, 1)
                            ).filter(t => t.piece.isDefined || board.enPassantTile.contains(t))
            forward ++ captures
        }
        shade match {
            case "Dark" => {
                pawnMove(South, SouthEast, SouthWest)
            }
            case "Light" => {
                pawnMove(North, NorthEast, NorthWest)
            }
        }
    }

    

    def col(range: Int = -1)(board: Board): Seq[Tile] = {
        val upRes = getTilesinDir(North, board, range)
        val downRes = getTilesinDir(South, board, range)
        upRes ++ downRes
    }

    def diagonal(range: Int = -1)(board: Board): Seq[Tile] = {
        val nE = getTilesinDir(NorthEast, board, range)
        val nW = getTilesinDir(NorthWest, board, range)
        val sE = getTilesinDir(SouthEast, board, range)
        val sW = getTilesinDir(SouthWest, board, range)
        nE ++ nW ++ sE ++ sW
    }

    def pony(range: Int = -1)(board: Board): Seq[Tile] = {
        val options = Seq(
            (1,2)  , (2,1),
            (-1,-2), (-2,-1),
            (-1,2) , (2,-1),
            (1,-2) , (-2,1)
        )

        options.flatMap{case (f, r) =>
            board.getTile((f+file).toChar,(r + rank))
        }
    }

    def getTilesinDir(dir: Direction, board: Board, range: Int = -1): Seq[Tile] = {
        board.getTile(neighborName(dir)) match {
            case Some(t) if t.piece.isDefined => Seq[Tile](t)
            case Some(t) => t.getTilesRec(dir, board, range)
            case None => Seq.empty[Tile]
        }
    }

    def getTilesRec(dir: Direction, board: Board, range: Int): Seq[Tile] = {
        val newRange = range - 1
        board.getTile(neighborName(dir)) match {
            case Some(t) if (newRange == 0) => Seq[Tile](this)
            case Some(t) if t.piece.isDefined => Seq[Tile](this, t)
            case Some(t) => this +: t.getTilesRec(dir, board, newRange) 
            case None  => Seq[Tile](this)
        }
    }

    def neighborName(dir: Direction): String = {
        dir.getTileName(name)
    }
}
