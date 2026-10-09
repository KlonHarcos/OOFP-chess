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
    private def getMove(shade: Shade, side: CastlingSide): Option[Notation] = {
        if get(shade, side) then Some(CastlingRights.kingMoveMap((shade, side))) else None
    }
    def getPotentialRookMove(from: Notation, to: Notation): Option[(Notation, Notation)] = CastlingRights.RookMove.get((from, to))

    def getKingMove(shade: Shade, side: CastlingSide): Option[Notation] = getMove(shade, side)


    def updated(from: Notation, to: Notation): CastlingRights = {
        if all.forall(b => b == false) then return this
        val l: Seq[(Shade, CastlingSide)] = Seq(from, to).flatMap(n =>
            CastlingRights.updateMap.get(n)).flatten

        l.foldLeft(this)((accum, pair) => accum.set(pair))
    }

    def setAll(value: Boolean): CastlingRights = {
        this.copy(blackKing = value, whiteKing = value, blackQueen = value, whiteQueen = value)
    }
    def set(pair: (Shade, CastlingSide), value: Boolean = false): CastlingRights = {
        pair match {
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

    private def all: Seq[Boolean] = Seq(whiteKing, whiteQueen, blackKing, blackQueen)
}

object CastlingRights {
    def apply(fenStr: String): CastlingRights = {
        CastlingRights(fenStr.contains('K'), fenStr.contains('Q'), fenStr.contains('k'), fenStr.contains('q'))
    }

    val updateMap: Map[Notation, Seq[(Shade, CastlingSide)]] = Map(
        ("e1", Seq((Light, KingSide),(Light, QueenSide))),
        ("a1", Seq((Light, QueenSide))),
        ("h1", Seq((Light, KingSide))),
        ("e8", Seq((Dark, KingSide), (Light, QueenSide))),
        ("a8", Seq((Dark, QueenSide))),
        ("h8", Seq((Dark, KingSide)))
    )

    val kingMoveMap: Map[(Shade, CastlingSide), Notation] = Map(
        ((Dark, KingSide), "g8"),
        ((Dark, QueenSide), "c8"),
        ((Light, KingSide), "g1"),
        ((Light, QueenSide), "c1")
    )

    val RookMove: Map[(Notation, Notation), (Notation, Notation)] = Map(
        (("e8", "g8"), ("h8", "f8")),
        (("e8", "c8"), ("a8", "d8")),
        (("e1", "g1"), ("h1", "f1")),
        (("e1", "c1"), ("a1", "d1"))
    )
    
}
