// package game

// import org.w3c.dom.Notation

// case class Move(from: Notation, to: Notation, isCastles: Option[Move] = None, isEnPassant: Option[Tile] = None) {
//     def ==(f: Notation, t: Notation): Boolean = {
//         this.from == f && this.to == t
//     }

//     def ==(f: Tile, t: Tile): Boolean = {
//         this.from == f.name && this.to == t.name
//     }

//     override def toString(): String = {
//         from +  to
//     }
// }

// object Move {
//     def enPassantFromString(fen: String): Option[Move] = {
//         if fen == "-" then return None
 
//         val takeTile = fen
//         val removeTileRank = if takeTile.rank == 6 then 5 else 4
//         Some(enPassant(takeTile, board.getTileUnsafe(takeTile.file.toString + removeTileRank.toString)))
//     }
// }