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
    val baseName: String
    def movePattern: Seq[Movement]
    var hasMoved: Boolean = false

    def name: String = shade match {
        case Dark => baseName.toLowerCase()
        case Light => baseName.toUpperCase()
    }

    def hasSameShade(piece: Option[Piece]): Boolean = piece match {
        case Some(p) if p.shade == this.shade => true 
        case None => false
        case _ => false
    }
}
case class King(shade: Shade) extends Piece {
    val baseName = "k"
    def movePattern: Seq[Movement] = Seq(
        Movement(_.king, shade, 1)
    )
}
case class Queen(shade: Shade) extends Piece {
    val baseName = "q"
    def movePattern: Seq[Movement] = Seq(
        Movement(_.row),
        Movement(_.col),
        Movement(_.diagonal),
    )
}
case class Rook(shade: Shade) extends Piece {
    val baseName = "r"
    def movePattern: Seq[Movement] = Seq(
        Movement(_.row),
        Movement(_.col)
    )
}
case class Knight(shade: Shade) extends Piece {
    val baseName = "n"
    def movePattern: Seq[Movement] = Seq(
        Movement(_.pony),
    )
}
case class Bishop(shade: Shade) extends Piece {
    val baseName = "b"
    def movePattern: Seq[Movement] = Seq(
        Movement(_.diagonal)
    )
}
case class Pawn(shade: Shade) extends Piece {
    val baseName = "p"
    def movePattern: Seq[Movement] = {
        def dist = if hasMoved then 1 else 2
        Seq(Movement(_.pawn, shade, dist))
    }
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

