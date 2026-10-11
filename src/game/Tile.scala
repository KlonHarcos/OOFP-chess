package game

import engine.graphics.{Color, Black, White, HighlightGreen}
import engine.graphics.Green
import engine.graphics.Position
import engine.graphics.Rectangle

sealed abstract trait Shade {
    val other: Shade
    val asString: String
}
case object Dark extends Shade {val other = Light; val asString: String = "Black"}
case object Light extends Shade{val other = Dark; val asString: String = "White"}


case class Tile(shade: Shade, rect: Rectangle, name: String, piece: Option[Piece] = None) {
    def rank: Int = name(1).asDigit
    def file: Char = name(0)


    def isInside(pos: Position): Boolean = {
        if (pos.x < rect.pos.x || pos.y < rect.pos.y) return false
        if (pos.x > rect.pos.x + rect.size || pos.y > rect.pos.y + rect.size) return false
        true
    }

    override def toString(): String = name

    //def getTilesinDir(dir: Direction, board: Board, range: Int = -1): Seq[Tile] = {
    //    board.getTile(neighborName(dir)) match {
    //        case Some(t) if t.piece.isDefined => Seq[Tile](t)
    //        case Some(t) => t.getTilesRec(dir, board, range)
    //        case None => Seq.empty[Tile]
    //    }
    //}

    //def getTilesRec(dir: Direction, board: Board, range: Int): Seq[Tile] = {
    //    val newRange = range - 1
    //    board.getTile(neighborName(dir)) match {
    //        case Some(t) if (newRange == 0) => Seq[Tile](this)
    //        case Some(t) if t.piece.isDefined => Seq[Tile](this, t)
    //        case Some(t) => this +: t.getTilesRec(dir, board, newRange) 
    //        case None  => Seq[Tile](this)
    //    }
    //}

    //def neighborName(dir: Direction): String = {
    //    dir.getTileName(name)
    //}
}
