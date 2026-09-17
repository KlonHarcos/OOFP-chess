package game

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
    val movePattern: Seq[Movement]
    var hasMoved: Boolean = false

    def name: String = shade match {
        case "Dark" => baseName.toLowerCase()
        case "Light" => baseName.toUpperCase()
    }
}
case class King(shade: Shade) extends Piece {
    val baseName = "k"
    val movePattern: Seq[Movement] = Seq(
        Movement(_.row, 1),
        Movement(_.col, 1),
        Movement(_.diagonal, 1),
    )
}
case class Queen(shade: Shade) extends Piece {
    val baseName = "q"
    val movePattern: Seq[Movement] = Seq(
        Movement(_.row),
        Movement(_.col),
        Movement(_.diagonal),
    )
}
case class Rook(shade: Shade) extends Piece {
    val baseName = "r"
    val movePattern: Seq[Movement] = Seq(
        Movement(_.row),
        Movement(_.col)
    )
}
case class Knight(shade: Shade) extends Piece {
    val baseName = "n"
    val movePattern: Seq[Movement] = Seq(
        Movement(_.pony),
    )
}
case class Bishop(shade: Shade) extends Piece {
    val baseName = "b"
    val movePattern: Seq[Movement] = Seq(
        Movement(_.diagonal)
    )
}
case class Pawn(shade: Shade) extends Piece {
    val baseName = "p"
    val movePattern: Seq[Movement] = hasMoved match {
        case true  => Seq(Movement(_.forward, shade, 1))
        case false => Seq(Movement(_.forward, shade, 2))
    }
}

object Piece {
    def getPiecebyName(name: Char): Piece = {
        val shade: Shade = if (name.isUpper) "Light" else "Dark"
        name.toLower match {
            case 'k' => King(shade)
            case 'q' => Queen(shade)
            case 'r' => Rook(shade)
            case 'n' => Knight(shade)
            case 'b' => Bishop(shade)
            case 'p' => Pawn(shade)
        }
    }
}
