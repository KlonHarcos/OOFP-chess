package game
object FenParser {
    def loadBoard(fen: String, board: Board): Unit = {
        val splitFen = fen.split(" ")
        val boardRows = splitFen(0).split("/")
        val nextToMove = splitFen(1)
        board.hasTurn = nextToMove match {
            case "w" => Light
            case "b" => Dark
        }
        val castlingRights = splitFen(2)
        board.castlingRights = board.castlingRights.setAll(false)
        castlingRights.foreach(c =>
            if (c == '-') () 
            else {
                val (shade, side) =  c match {
                    case 'k' | 'K' => val s = if c.isUpper then Light else Dark; (s, KingSide)
                    case 'q' | 'Q' => val s = if c.isUpper then Light else Dark; (s, QueenSide) 
                }
                board.castlingRights = board.castlingRights.set(shade, side, true)}
        )
        val enPassantSquare = splitFen(3)
        board.enPassantMove = if enPassantSquare == "-" then None else 
            val takeTile = board.getTileUnsafe(enPassantSquare)
            val removeTileRank = if takeTile.rank == 6 then 5 else 4
            Some(enPassant(takeTile, board.getTileUnsafe(takeTile.file.toString + removeTileRank.toString)))
        val halfMoveClock = splitFen(4)
        val fullMoveClock = splitFen(5)

        for ((rowStr, index) <- boardRows.reverse.zipWithIndex) {
            val rowNum: Int = index + 1
            var colNum: Int = 0
            def colLetter: Char = ('a' + colNum).toChar
            for (c <- rowStr) {
                if (c.isDigit) colNum += c.asDigit
                else {
                    val p: Piece = Piece.getPiecebyName(c)
                    val strTileName = colLetter.toString + rowNum
                    board.placePiece(strTileName, Some(p))
                    if (p.isInstanceOf[Pawn]) {
                        val startingRank = p.shade match {
                            case Dark => 7
                            case Light => 2
                        }
                        if (rowNum != startingRank) board.placePiece(strTileName, Some(p.move()))
                    }
                    
                    colNum += 1
                }
            }
        }

        board.getTiles().foreach{t =>
            t.piece match
                case Some(King(Dark, _)) => board.blackKingTile = t.name
                case Some(King(Light, _)) => board.whiteKingTile = t.name
                case _ => ()
        }
    }
}
