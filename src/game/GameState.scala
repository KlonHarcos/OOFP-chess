package game

import scala.collection.mutable.Map as MMap

case class GameState(
    hasTurn: Shade,
    enPassantMove: Option[Move],
    CastlingRights: CastlingRights,
    piecePositions: Map[Piece, Notation],
    pieceVision: Map[Piece, Set[Notation]]
) {
    private def changeTurn: GameState = this.copy(hasTurn = hasTurn.other)
    private lazy val opponentVision: Map[Piece, Notation] = piecePositions.collect {
        case (k, v) if k.shade == hasTurn.other => (k, v)
    }

    def tryMove(from: Notation, to: Notation): GameState = {
        val p = invertedPieceMap.get(from)
        p match {
            case Some(piece) if getVisionFor(piece).contains(to) && piece.shade == hasTurn =>
                if isDangerous(from, to) then return this
                makeMove(piece, to)
            case _ => this
        }
    }

    private def setVision(): GameState = {
        val occupiedSquares = piecePositions.values.toSet
        val pairs = piecePositions.keys.map{ piece => 
            val pos = piecePositions(piece)
            val naiveVision = piece.movePattern(pos)
            val vision: Set[Notation] = naiveVision.flatMap{ s =>
                if (s.isEmpty) Seq.empty[Notation]
                val find = s.indexWhere(s => occupiedSquares.contains(s))
                if find == -1 then s 
                else s.take(find + 1)
            }
            (piece, vision)
        }.toSeq

        val visMap = Map.from(pairs)

        this.copy(pieceVision = visMap)
    }

    private def recalculateVision(piece: Piece, pMap: Map[Piece, Notation]): (Piece, Set[Notation]) = {
        val pos: Notation = pMap(piece)
        val occupiedSquares: Set[Notation] = pMap.values.toSet
        val naiveVision: Set[Seq[Notation]] = piece.movePattern(pos)
        val vision: Set[Notation] = naiveVision.flatMap{ s =>
            if (s.isEmpty) Seq.empty[Notation]
            val find = s.indexWhere(s => occupiedSquares.contains(s))
            if find == -1 then s 
            else s.take(find + 1)
        }
        (piece, vision)
    }

    def getVisionFor(piece: Option[Piece]): Set[Notation] = piece match {
        case Some(p) => pieceVision(p)
        case None => Set.empty[Notation]
    }

    private def getVisionFor(piece: Piece): Set[Notation] = pieceVision(piece)

    private def tilesUnderAttack(shade: Shade): Set[Notation] = shade match {
        case Dark => whiteTargets
        case Light => blackTargets
    }

    lazy val (whiteTargets, blackTargets) = {
        val parts = pieceVision.partition{ case (p, _) => p.shade == Light}
        (parts._1.valuesIterator.flatten.toSet, parts._2.valuesIterator.flatten.toSet)
    }

    private def isTileUnderAttack(tile: Notation): Boolean = tilesUnderAttack(hasTurn).contains(tile)

    private def isDangerous(from: Notation, to: Notation): Boolean = {
        val piece = invertedPieceMap(from)
        val isToUnderAttack = isTileUnderAttack(to)
        
        if piece.isInstanceOf[King] && isToUnderAttack then return true
        if !isTileUnderAttack(from) && !isInCheck(hasTurn) then return false

        //most expensive calc
        val newState = this.makeMove(piece, to)
        //if isInCheck(hasTurn) && newState.isInCheck(hasTurn) then return true
        if newState.isInCheck(this.hasTurn) then return true
        else return false
    }

    private def isInCheck(shade: Shade): Boolean = {
        val kingPos = getKingPos(shade)
        tilesUnderAttack(shade).contains(kingPos)
    } 

    lazy val whiteKing: King = piecePositions.keys.collectFirst {case k: King if k.shade == Light => k}
        .getOrElse(throw new Exception("White king is not on board"))    
    
    lazy val blackKing: King = piecePositions.keys.collectFirst {case k: King if k.shade == Dark => k}
        .getOrElse(throw new Exception("White king is not on board"))

    private def getKingPos(shade: Shade): Notation = shade match {
        case Dark => piecePositions(blackKing)
        case Light => piecePositions(whiteKing)
    }


    val invertedPieceMap = piecePositions.map(_.swap)
    def getPseudoLegalMoves(piece: Option[Piece]): Set[Notation] = {
        val vision = getVisionFor(piece)
        piece match {
            case Some(p @Pawn(shade, id)) => vision.filter(sqr => 
                val sameFileAsPawn = sqr.file == piecePositions(p).file
                invertedPieceMap.get(sqr) match {
                    case None if sameFileAsPawn => true
                    case Some(other) if !sameFileAsPawn && other.shade != p.shade  => true
                    case _ => false
                }
            )
            case Some(p) => vision.filter(sqr => invertedPieceMap.get(sqr) match {
                case Some(other) if other.shade == p.shade => false
                case _ => true
            })
            case _ => Set.empty[Notation]
        }

    }

    def updatedVisions(from: Notation, to: Notation, pMap: Map[Piece, Notation]): Map[Piece, Set[Notation]] = {
        pieceVision.flatMap{case (k,v) =>
            if !pMap.get(k).isDefined then None // filter out removed piece
            else if v.contains(from) || v.contains(to) then Some(recalculateVision(k, pMap))
            else Some(k,v)
        }
    }

    private def makeMove(p: Piece, to: Notation): GameState = {
        if p.shade != hasTurn then throw new Exception("illegal piece is making a move")
        val newPMap = removePieceAt(to).updated(p, to)
        this.copy(
            hasTurn = hasTurn.other, 
            pieceVision = updatedVisions(piecePositions(p), to, newPMap),
            piecePositions = newPMap
        )
    }

    private def removePieceAt(target: Notation): Map[Piece, Notation] = {
        val p = invertedPieceMap.get(target)
        p match {
            case Some(value) => piecePositions.removed(value)
            case None => piecePositions
        }
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