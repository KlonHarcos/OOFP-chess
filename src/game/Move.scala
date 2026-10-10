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

case class enPassant(pawnTarget: Notation, pawnToRemove: Notation)