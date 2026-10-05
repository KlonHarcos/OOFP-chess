package game

import scala.collection.mutable.Map as MMap


case class GameState(
    hasTurn: Shade,
    enPassantMove: Option[Move],
    CastlingRights: CastlingRights,
    piecePostisions: Map[Piece, Notation],
    pieceVision: Map[Piece, Set[Notation]]
) {
    def changeTurn: GameState = this.copy(hasTurn = hasTurn.other)

    def setVision(): GameState = {
        val pairs = piecePostisions.keys.map{ piece => 
            val vision = piece.movePattern
            (piece,vision)
        }.toSeq

        val visMap = Map.from(pairs)

        this.copy(pieceVision = visMap)
    } 
}


object GameState {
    def apply(fen: String): GameState = {
        val splitFen = fen.split(" ")
        val boardRows = splitFen(0).split("/")
        val nextToMove = splitFen(1)
        
        val hasTurn = nextToMove match {
            case "w" => Light
            case "b" => Dark
        }
        
        val castlingRightsStr = splitFen(2)
        var castlingRights = CastlingRights(castlingRightsStr)


        val enPassantStr = splitFen(3)
        val enPassantMove = Move.enPassantFromString(enPassantStr)
        
        val halfMoveClock = splitFen(4)
        val fullMoveClock = splitFen(5)

        def indexAsNotation(i: Int): String = {
            val rank = 8 - i / 8
            val file = ('a' + (i % 8)).toChar.toString
            file + rank.toString
        }

        val (pairs, tileIndex, numPiece) = boardRows.flatten.toSeq.foldLeft((Seq.empty[(Piece, Notation)], 0: Int , 0: PieceId)) {case ((accum, tileId, nP), c) =>
            c match {
                case skippedTiles if skippedTiles.isDigit => (accum, tileId + skippedTiles.asDigit, nP)
                case a =>{
                    val piece = Piece.createPiecebyName(a, nP)
                    val tileName = indexAsNotation(tileId)
                    (accum.appended((piece, tileName)),tileId + 1, nP + 1)
                } 
            }
        }
        val pieceMap = Map.from(pairs)

        val noVis =GameState(
            hasTurn,
            enPassantMove,
            castlingRights,
            pieceMap,
            Map.empty[Piece, Set[Notation]]
        )

        noVis.setVision()
    }
}