package game

import org.w3c.dom.Notation

case class Move(from: Notation, to: Notation, isCastles: Option[Move] = None, isEnPassant: Option[Tile] = None) {
    def ==(f: Notation, t: Notation): Boolean = {
        this.from == f && this.to == t
    }

    def ==(f: Tile, t: Tile): Boolean = from.toString == f.name && to.toString == t.name

    override def toString(): String = {
        from.toString +  to.toString
    }
}

case class enPassant(pawnTarget: Notation, pawnToRemove: Notation)


object Move {
    def enPassantFromString(fen: String): Option[Move] = {
        None
        // if fen == "-" then return None
 
        // val takeTile: Notation = fen
        // val removeTileRank = if takeTile.rank == 6 then 5 else 4
        // Some(enPassant(takeTile, Notation(takeTile.file, removeTileRank.toString))
    }
}