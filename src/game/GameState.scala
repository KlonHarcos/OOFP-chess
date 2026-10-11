package game

import scala.collection.mutable.Map as MMap

type PieceMap = Map[Piece, Notation]
type gameOverMessage = Either[String, String]

case class GameState(
    hasTurn: Shade,
    enPassantSquare: Option[Notation],
    castlingRights: CastlingRights,
    piecePositions: PieceMap,
    pieceVision: Map[Piece, Set[Notation]],
    halfMoveClock: Int,
    fullMoveClock: Int
) {
    private lazy val opponentVision: PieceMap = piecePositions.collect {
        case (k, v) if k.shade == hasTurn.other => (k, v)
    }
    
    private val invertedPieceMap = piecePositions.map(_.swap)
    
    private lazy val (whiteTargets, blackTargets) = {
        val parts = pieceVision.partition{ case (p, _) => p.shade == Light}
        (parts._1.valuesIterator.flatten.toSet, parts._2.valuesIterator.flatten.toSet)
    }

    private lazy val whiteKing: Option[King] = findKingSquare(Light)
    private lazy val blackKing: Option[King] = findKingSquare(Dark)
    private def findKingSquare(shade: Shade): Option[King] = piecePositions.keysIterator.collectFirst {case k: King if k.shade == shade => k}

    extension (i: PieceMap) {
        private def removePieceAt(attacker: Piece, target: Notation): PieceMap = { 
            val p = invertedPieceMap.get(target)
            p match {
                case None if isEnPassantSquare(target) && attacker.isInstanceOf[Pawn] => {
                    val newTarget = hasTurn match {
                        case Dark => target.at(North)
                        case Light => target.at(South)
                    }
                    removePieceAt(attacker, newTarget)
                }
                case Some(value) => piecePositions.removed(value)
                case None => piecePositions
            }
        }

        private def removePieceAt(attacker: Piece, target: Option[Notation]): PieceMap = target match {
            case None => piecePositions
            case Some(value) => removePieceAt(attacker, value)
        }


        private def updatePiecePos(piece: Piece, target: Notation): PieceMap = {
            piece match {
                case p @ Pawn(shade, id) if target.rank == p.promotionRank => i.updated(Queen(shade, id), target).removed(p)
                case piece => i.updated(piece, target)
            }
        }

        private def castledRookMoved(optKing: Piece, to: Notation): PieceMap = {
            if !optKing.isInstanceOf[King] then return i
            val from = i(optKing)
            val rookMove: Option[(Notation, Notation)] = castlingRights.getPotentialRookMove(from, to)
            rookMove match {
                case Some(movePair) => i.updated(invertedPieceMap(movePair._1), movePair._2)
                case None => i
            }
        }
    }

    def getWinner(): Option[gameOverMessage] = {
        if halfMoveClock == 100 then return Some(Left("move count"))
        if isCheckMate then return Some(Right(hasTurn.other.asString))
        if isStaleMate then return Some(Left("stale-mate"))
        None
    }

    def isCheckMate: Boolean = allLegalMoves.isEmpty && isInCheck(hasTurn)
    def isStaleMate: Boolean = allLegalMoves.isEmpty && !isInCheck(hasTurn)

    def isGameOver: Boolean = !whiteKing.isDefined || !blackKing.isDefined || getWinner().isDefined

    lazy val allLegalMoves: Set[Move] = allPieces.collect{case p if p.shade == hasTurn =>
        val from = piecePositions(p)
        getLegalTargetTiles(Some(p)).map(to => Move(from, to))
    }.flatten

    lazy val allPseudoLegalMoves = allPieces.collect{case p if p.shade == hasTurn =>
        val from = piecePositions(p)
        getPseudoLegalMoves(p).map(to => Move(from, to))
    }.flatten



    def getLegalTargetTiles(p: Option[Piece]): Set[Notation] = p match {
        case Some(piece) if piece.shade == hasTurn =>  getPseudoLegalMoves(piece).filterNot(isDangerous(piecePositions(piece), _))
        case _ => Set.empty[Notation]
    }

    lazy val allPieces: Set[Piece] = piecePositions.keys.toSet  

    def tryMove(from: Notation, to: Notation): GameState = {
        val p = invertedPieceMap.get(from)
        
        p match {
            case Some(piece) if piece.shade == hasTurn && getLegalTargetTiles(p).contains(to)  =>
                makeMove(piece, to)
            case None => println("No piece at this pos"); this
            case _ => println(f"${Move(from,to)} is illegal on ${hasTurn.asString}'s turn"); this
        }
    }

    def tryMoveUnsafe(from: Notation, to: Notation): GameState = {
        val p = invertedPieceMap.get(from)
        
        p match {
            case Some(piece) if piece.shade == hasTurn  =>
                makeMove(piece, to)
            case None => println("No piece at this pos"); this
            case _ => println(f"${Move(from,to)} is illegal on ${hasTurn.asString}'s turn"); this
        }
    }

    private def setVision(): GameState = {
        val occupiedSquares = piecePositions.values.toSet
        val visMap = piecePositions.map{ (piece, pos) => 
            recalculateVision(piece, piecePositions)
        }
        this.copy(pieceVision = visMap)
    }


    //passed in pmap is necessary so that it checks the future GameState Positions
    private def recalculateVision(piece: Piece, pMap: PieceMap): (Piece, Set[Notation]) = {
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

    private def getVisionFor(piece: Option[Piece]): Set[Notation] = piece match {
        case Some(p) => pieceVision(p)
        case None => Set.empty[Notation]
    }

    private def getVisionFor(piece: Piece): Set[Notation] = pieceVision(piece)

    private def tilesUnderAttack(shade: Shade): Set[Notation] = shade match {
        case Dark => whiteTargets
        case Light => blackTargets
    }

    
    private def isTileUnderAttack(tile: Notation): Boolean = tilesUnderAttack(hasTurn).contains(tile)

    private def isDangerous(from: Notation, to: Notation): Boolean = {
        val piece = invertedPieceMap(from)
        val isToUnderAttack = isTileUnderAttack(to)
        
        if piece.isInstanceOf[King] && isToUnderAttack then return true
        if !isTileUnderAttack(from) && !isInCheck(hasTurn) then return false

        //most expensive calc
        val newState = this.makeMove(piece, to)
        if newState.isInCheck(this.hasTurn) then return true
        else return false
    }

    def isInCheck(shade: Shade): Boolean = {
        val kingPos = getKingPos(shade)
        kingPos match {
            case None => true
            case Some(value) => tilesUnderAttack(shade).contains(value)
        }
    } 

    private def getKingPos(shade: Shade): Option[Notation] = {
        val k = shade match {
            case Dark => blackKing
            case Light => whiteKing
        }
        k match {
            case None => None
            case Some(value) => Some(piecePositions(value))
        }
    }


    private def getPseudoLegalMoves(piece: Piece): Set[Notation] = {
        val vision = getVisionFor(piece)
        piece match {
            case p @Pawn(shade, id) => vision.filter(sqr => 
                val sameFileAsPawn = sqr.file == piecePositions(p).file
                invertedPieceMap.get(sqr) match {
                    case None if sameFileAsPawn => true
                    case None if !sameFileAsPawn && isEnPassantSquare(sqr) => true
                    case Some(other) if !sameFileAsPawn && other.shade != p.shade  => true
                    case _ => false
                }
            )
            case k @King(shade, _) => (vision ++ getLegalCastles(shade)).filter(sqr => invertedPieceMap.get(sqr) match {
                case Some(other) if other.shade == k.shade => false
                case _ => true
                })
            case p => vision.filter(sqr => invertedPieceMap.get(sqr) match {
                case Some(other) if other.shade == p.shade => false
                case _ => true
                })
        }

    }

    private def getCastleMove(shade: Shade, side: CastlingSide): Option[Notation] = {

       if isInCheck(shade) then return None

       if !castlingRights.get(shade, side) then return None

       val freeTiles = (shade, side) match {
           case (Dark, KingSide) => Seq("f8", "g8")
           case (Dark, QueenSide) => Seq("b8", "c8", "d8")
           case (Light, KingSide) => Seq("f1", "g1")
           case (Light, QueenSide) => Seq("b1", "c1", "d1")
           case input => throw new IllegalArgumentException(s"Unexpected input: $input")
       }

       if freeTiles.exists(n => isTileUnderAttack(n)) then return None

       return castlingRights.getKingMove(shade, side)
    }

    private def getLegalCastles(shade: Shade): Set[Notation] = Set(getCastleMove(shade, KingSide), getCastleMove(shade, QueenSide)).flatten

    private def isEnPassantSquare(target: Notation): Boolean = enPassantSquare match {
        case Some(n) => n == target
        case None => false 
    }

    private def updatedVisions(from: Notation, to: Notation, pMap: PieceMap): Map[Piece, Set[Notation]] = pMap.map{case (piece, newPos) =>
        //Knight vision stays regardless if it hasnt moved
        if piece.isInstanceOf[Knight] && newPos != to then (piece, pieceVision(piece)) 
        else {
            val currVis = pieceVision.get(piece)
            currVis match {
                //None means piece appeared == promotion
                case None => recalculateVision(piece, pMap) 
                case Some(vis) if vis.contains(from) || vis.contains(to) => recalculateVision(piece, pMap)
                case Some(vis) => (piece, vis)
            }
        }
    }

    private def makeMove(p: Piece, to: Notation): GameState = {
        if p.shade != hasTurn then throw new Exception("illegal piece is making a move")
        val from = piecePositions(p)
        val newPMap = piecePositions.removePieceAt(p, to).castledRookMoved(p, to).updatePiecePos(p, to)
        val isCapture = invertedPieceMap.get(to).isDefined 
        val enPass: Option[Notation] = p match {
            case pawn @ Pawn(_, _) if pawn.didJump(from, to) => Some(pawn.getEnPassantSqr(from))
            case _ => None
        }
        val res = this.copy(
            hasTurn = hasTurn.other, 
            pieceVision = updatedVisions(from, to, newPMap),
            piecePositions = newPMap,
            castlingRights = castlingRights.updated(from, to),
            enPassantSquare = enPass,
            halfMoveClock = if p.isInstanceOf[Pawn] || isCapture then 0 else halfMoveClock + 1,
            fullMoveClock = if hasTurn == Dark then fullMoveClock + 1 else fullMoveClock
        )
        res
    }

    override def toString(): String = {
       val enPass: String = enPassantSquare match {
           case None => "-"
           case Some(value) => value
       }
       val nextToMove = hasTurn match {
           case Light => "w"
           case Dark => "b"
       }

       Seq[String](boardAsString(), nextToMove, castlingRights.toString, enPass, halfMoveClock.toString, fullMoveClock.toString).mkString(" ")
    }

    private def boardAsString(): String = {var strBuilder = ""
       val ranks = Notation.all.grouped(8)
       for (rank <- ranks) {
           var emptyCounter = 0
           for (sqr <- rank) {
                invertedPieceMap.get(sqr) match {
                    case None => emptyCounter += 1
                    case Some(p) if emptyCounter == 0 => strBuilder += p.name
                    case Some(p) => {
                        strBuilder += emptyCounter.toString + p.name
                        emptyCounter = 0
                    } 
                }
            }
            if emptyCounter != 0 then strBuilder += emptyCounter
            strBuilder += "/"
       }
       strBuilder = strBuilder.dropRight(1)
       strBuilder
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
        val enPassantMove: Option[Notation] = if enPassantStr == "-" then None else Some(splitFen(3))
        
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

        val noVis = GameState(
            hasTurn,
            enPassantMove,
            castlingRights,
            pieceMap,
            Map.empty[Piece, Set[Notation]],
            halfMoveClock.toInt,
            fullMoveClock.toInt
        )

        noVis.setVision()
    }
}