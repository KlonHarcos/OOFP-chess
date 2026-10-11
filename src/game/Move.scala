package game

case class Move(from: Notation, to: Notation) {
    def ==(f: Notation, t: Notation): Boolean = {
        this.from == f && this.to == t
    }

    def ==(f: Tile, t: Tile): Boolean = from.toString == f.name && to.toString == t.name

    override def toString(): String = {
        from.toString +  to.toString
    }
}

object Move {
    def fromString(str: String): Move = {
        val seq = str.grouped(2).toSeq
        Move(seq(0), seq(1))
    }

    def empty: Move = Move("", "")
}