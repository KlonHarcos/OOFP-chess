package game


abstract class Piece() {
    val shade: Shade
    val baseName: String

    def name: String = shade match {
        case "Dark" => baseName.toLowerCase()
        case "Light" => baseName.toUpperCase()
    }
}
case class King(shade: Shade) extends Piece {
    val baseName = "k"
}
case class Queen(shade: Shade) extends Piece {
    val baseName = "q"
}
case class Rook(shade: Shade) extends Piece {
    val baseName = "r"
}
case class Knight(shade: Shade) extends Piece {
    val baseName = "n"
}
case class Bishop(shade: Shade) extends Piece {
    val baseName = "b"
}
case class Pawn(shade: Shade) extends Piece {
    val baseName = "p"
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
