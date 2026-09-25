package game

import dotty.tools.dotc.util.optional

type Movement = Tile => Board => Seq[Tile]

object Movement {
    def apply(func: Tile => Int => Board => Seq[Tile], range: Int = -1): Movement = {
        tile => func(tile)(range)
    }
    def apply(func: Tile => (Shade, Int) => Board => Seq[Tile], shade: Shade, range: Int): Movement = {
        tile => func(tile)(shade, range)
    }
}

abstract class Piece() {
    val shade: Shade
    val hasMoved: Boolean
    val baseName: String
    val value: Int
    def movePattern: Seq[Movement]

    def name: String = shade match {
        case Dark => baseName.toLowerCase()
        case Light => baseName.toUpperCase()
    }

    def hasSameShade(piece: Option[Piece]): Boolean = piece match {
        case Some(p) if p.shade == this.shade => true 
        case None => false
        case _ => false
    }

    def move(): Piece
}
case class King(shade: Shade, hasMoved: Boolean = false) extends Piece {
    val baseName = "k"
    val value = 100
    def movePattern: Seq[Movement] = Seq(
        Movement(_.row, 1),
        Movement(_.col, 1),
        Movement(_.diagonal, 1)
    )

    def move(): King = this.copy(hasMoved = true)
}
case class Queen(shade: Shade, hasMoved: Boolean = false) extends Piece {
    val baseName = "q"
    val value = 9
    def movePattern: Seq[Movement] = Seq(
        Movement(_.row),
        Movement(_.col),
        Movement(_.diagonal),
    )

    def move(): Queen = this.copy(hasMoved = true)
}
case class Rook(shade: Shade, hasMoved: Boolean = false) extends Piece {
    val baseName = "r"
    val value = 5
    def movePattern: Seq[Movement] = Seq(
        Movement(_.row),
        Movement(_.col)
    )

    def move(): Rook = this.copy(hasMoved = true)
}
case class Knight(shade: Shade, hasMoved: Boolean = false) extends Piece {
    val baseName = "n"
    val value = 3
    def movePattern: Seq[Movement] = Seq(
        Movement(_.pony),
    )

        def move(): Knight = this.copy(hasMoved = true)

}
case class Bishop(shade: Shade, hasMoved: Boolean = false) extends Piece {
    val baseName = "b"
    val value = 3
    def movePattern: Seq[Movement] = Seq(
        Movement(_.diagonal)
    )

    def move(): Bishop = this.copy(hasMoved = true)

}
case class Pawn(shade: Shade, hasMoved: Boolean = false) extends Piece {
    val baseName = "p"
    val value = 1
    def movePattern: Seq[Movement] = {
        def dist = if hasMoved then 1 else 2
        Seq(Movement(_.pawn, shade, dist))
    }

    def move(): Pawn = this.copy(hasMoved = true)

}

object Piece {
    def getPiecebyName(name: Char): Piece = {
        val shade: Shade = if (name.isUpper) Light else Dark
        name.toLower match {
            case 'k' => King(shade)
            case 'q' => Queen(shade)
            case 'r' => Rook(shade)
            case 'n' => Knight(shade)
            case 'b' => Bishop(shade)
            case 'p' => Pawn(shade)
        }
    }

    def hasSameColor(a: Option[Piece], b: Option[Piece]) = {
        (a, b) match {
            case (Some(p), Some(p2)) if p.shade == p2.shade => true
            case _ => false
        }
    }
}

