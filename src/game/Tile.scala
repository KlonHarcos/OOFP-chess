package game

import engine.graphics.{Color, Black, White, HighlightGreen}
import engine.graphics.Green
import engine.graphics.Position
import engine.graphics.Rectangle

type Shade = "Dark" | "Light"


case class Tile(shade: Shade, rect: Rectangle, name: String, piece: Option[Piece] = None) {
    def isInside(pos: Position): Boolean = {
        if (pos.x < rect.pos.x || pos.y < rect.pos.y) return false
        if (pos.x > rect.pos.x + rect.size || pos.y > rect.pos.y + rect.size) return false
        true
    }

    def nameAsNotation(): Notation = (name(0), name(1).toString.toInt)
    //def get
}