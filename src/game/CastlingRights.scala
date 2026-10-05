package game

sealed trait CastlingSide
case object KingSide extends CastlingSide
case object QueenSide extends CastlingSide

case class CastlingRights(
    whiteKing: Boolean = true,
    whiteQueen: Boolean = true,
    blackKing: Boolean = true,
    blackQueen: Boolean = true
) {
    //def getMove(shade: Shade, side: CastlingSide, board: Board): Move = {
    //    val Seq(kf, kt, rf, rt) = ((shade, side) match {
    //        case (Dark, KingSide) =>  Seq("e8", "g8", "h8", "f8")
    //        case (Dark, QueenSide) => Seq("e8", "c8", "a8", "d8")
    //        case (Light, KingSide) => Seq("e1", "g1", "h1", "f1")
    //        case (Light, QueenSide) => Seq("e1", "c1", "a1", "d1")
    //        case input => throw new IllegalArgumentException(s"Unexpected input: $input")
    //    }).map(board.getTileUnsafe(_))
    //    Move(kf, kt, isCastles = Some(Move(rf, rt)))
    //}

    def setAll(value: Boolean): CastlingRights = {
        this.copy(blackKing = value, whiteKing = value, blackQueen = value, whiteQueen = value)
    }
    def set(shade: Shade, side: CastlingSide, value: Boolean = false): CastlingRights = {
        (shade, side) match {
            case (Dark, KingSide) => this.copy(blackKing = value)
            case (Dark, QueenSide) => this.copy(blackQueen = value)
            case (Light, KingSide) => this.copy(whiteKing = value)
            case (Light, QueenSide) => this.copy(whiteQueen = value)
            case input => throw new IllegalArgumentException(s"Unexpected input: $input")
        }
    }

    def get(shade: Shade, side: CastlingSide): Boolean = (shade, side) match {
        case (Light, KingSide) => whiteKing
        case (Light, QueenSide) => whiteQueen
        case (Dark, KingSide) =>  blackKing
        case (Dark, QueenSide) => blackQueen
        case input => throw new IllegalArgumentException(s"Unexpected input: $input")

    }

    override def toString() = {
        val str = Seq((whiteKing, "K"), (whiteQueen, "Q"), (blackKing, "k"), (blackQueen, "q")).map{ case (b, s) =>
            if b then s else ""
        }.mkString

        if str.isEmpty then "-" else str
    }

    def fromString(str: String): CastlingRights = {
        if str == "-" then return CastlingRights(false, false, false, false)
        var right = CastlingRights()
        str.foreach{c => 
            val side = if c.toLower == 'k' then KingSide else QueenSide
            val shade: Shade = if c.isLower then Dark else Light
            right = right.set(shade, side)
        }
        right
    }
}

object CastlingRights {
    def apply(fenStr: String): CastlingRights = {
        CastlingRights(fenStr.contains('K'), fenStr.contains('Q'), fenStr.contains('k'), fenStr.contains('q'))
    }
}
