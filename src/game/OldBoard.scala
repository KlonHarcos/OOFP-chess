//package game
//
//import engine.graphics.{Rectangle, Position, Black, White}
//import game.{Shade, Dark, Light}
//import scala.collection.mutable.Map as MMap
//
//case class Move(from: Tile, to: Tile, isCastles: Option[Move] = None, isEnPassant: Option[Tile] = None) {
//    def ==(f: Tile, t: Tile): Boolean = {
//        this.from == f && this.to == t
//    }
//
//    override def toString(): String = {
//        from.name + to.name
//    }
//}
//
//case class enPassant(pawnTarget: Tile, pawnToRemove: Tile)
//
//
//class Board(val tiles: MMap[Notation, Tile]) {
//
//    //def gameState(): String = {
//    //    val noLegalMoves = getAllLegalMoves(hasTurn).isEmpty
//    //    if noLegalMoves && isInCheck(hasTurn) then "Winner:" + hasTurn.other
//    //    else if noLegalMoves then "Draw"
//    //    else "running" 
//    //}
//
//    //def getVision(shade: Shade): Seq[Tile] = shade match {
//    //    case Dark => visionDark
//    //    case Light => visionLight
//    //}
//
//    //def initializeVision(): Unit = {
//    //    val pairs = piecePostisions.values.map(tile => 
//    //        tile.piece match {
//    //            case Some(p) => Some(p.id, getVistionAt(tile))
//    //            case _ => None
//    //        }
//    //    ).flatMap(identity)
//    //    pairs.foreach((key, value) => pieceVision.update(key, value))
//    //}
//
//    //def updateVision(m: Move): Unit = {
//    //    for(pId <- pieceVision.keys) {
//    //        Seq(m.from, m.to).foreach {tile =>
//    //            if pieceVision(pId).contains(tile) then pieceVision.update(pId, getVistionAt(piecePostisions(pId)))
//    //        }
//    //    }
//    //}
//
//    def updatePiecePos(p: Option[Piece], newPos: Option[Tile]): Unit = {
//        (p, newPos) match {
//            case (Some(p), Some(t)) => piecePostisions.update(p.id, t)
//            case (Some(p), None) => piecePostisions.remove(p.id)
//            case _ => ()
//        }
//
//    }
//
//    def isUnderAttack(shadeUnderAttack: Shade, tile: Tile): Boolean = {
//        //getVision(shadeUnderAttack.other).contains(tile)
//        false
//    }
//
//    def getCastleMove(shade: Shade, side: CastlingSide): Option[Move] = {
//
//        if isInCheck(shade) then return None
//
//        if !castlingRights.get(shade, side) then return None
//
//        val freeTiles = (shade, side) match {
//            case (Dark, KingSide) => Seq("f8", "g8")
//            case (Dark, QueenSide) => Seq("b8", "c8", "d8")
//            case (Light, KingSide) => Seq("f1", "g1")
//            case (Light, QueenSide) => Seq("b1", "c1", "d1")
//            case input => throw new IllegalArgumentException(s"Unexpected input: $input")
//        }
//
//        val tilesToCheck = freeTiles.map(getTileUnsafe(_))
//
//        val tilesAreFree = tilesToCheck.forall(_.piece == None)
//        if !tilesAreFree then return None
//
//        val tilesAreUnderAttack = tilesToCheck.exists(isUnderAttack(shade, _))
//        if tilesAreUnderAttack then return None
//
//        return Some(castlingRights.getMove(shade, side, this))
//    }
//
//    def getLegalCastles(shade: Shade): Seq[Move] = {
//        Seq(getCastleMove(shade, KingSide), getCastleMove(shade, QueenSide)).flatMap(identity)
//    }
//
//    def getPossibleTargetTile(tile: Tile): Seq[Tile] = getLegalMoves(tile).map(m => m.to)
//    //def getTilesInVision(tile: Tile) = getPseudoLegalMoves(tile).map(m => m.to)
//
//    def getVistionAt(tile: Tile): Set[Tile] = {
//        tile.piece match {
//            case Some(p) => p.movePattern.flatMap(f => f(tile)(this)).toSet
//            case _ => Set.empty[Tile]
//        }
//    }
//
//    def getPseudoLegalMoves(tile: Tile): Seq[Move] = {
//        val currPiece = tile.piece
//        val possible = tile.piece match {
//            case None => Seq.empty[Tile]
//            case Some(p) => {
//                p.movePattern.flatMap(f  => f(tile)(this))}
//        }
//        possible.filterNot(t => Piece.hasSameColor(t.piece, currPiece))
//                    .map{to => Move(tile, to)}
//    }
//
//    def getLegalMoves(tile: Tile): Seq[Move] = {
//        val currPiece = tile.piece
//        val pseudoLegal = getPseudoLegalMoves(tile)            
//        
//        val legal = pseudoLegal.filterNot(isDangerous(_))
//        println("hi")
//        currPiece match {
//            case Some(King(s, _)) => legal ++ getLegalCastles(s)
//            case Some(Pawn(_, _)) => legal ++ getLegalEnPassant(tile)
//            case _ => legal                 
//        }
//    }
//
//    def getLegalEnPassant(tile: Tile): Seq[Move] = {
//        enPassantMove match {
//            case None => Seq.empty[Move]
//            case Some(enPass) => {
//                val diff = (enPass.pawnToRemove.file - tile.file)
//                if ((diff == 1 || diff == -1) && tile.rank == enPass.pawnToRemove.rank) then Seq(Move(tile, enPass.pawnTarget, isEnPassant = Some(enPass.pawnToRemove)))
//                else Seq.empty[Move]
//            }
//        }
//    }
//
//    def getAllLegalMoves(shade: Shade): Seq[Move] = {
//        val playerTiles = getTiles().filter(t => t.piece.exists(_.shade == shade))
//        playerTiles.flatMap(getLegalMoves(_))
//    }
//
//    def isDangerous(move: Move): Boolean = {
//        false
//        //val tempBoard = this.deepCopy()
//        //val currTurn = tempBoard.hasTurn
//        //tempBoard.movePiece(move)
//        //tempBoard.isInCheck(currTurn)
//    }
//
//    def isInCheck(shade: Shade): Boolean = {
//        def getKing(shade: Shade): Tile = {
//            shade match {
//                case Dark => piecePostisions(blackKingId)
//                case Light => piecePostisions(whiteKingId)
//            }
//        }
//        val kingTile = getKing(shade)
//        isUnderAttack(shade, kingTile)
//    }
//
//    def placePiece(tile: Tile, p: Option[Piece]) = {
//        tiles(tile.name) = tile.copy(piece = p)
//    }
//
//    def placePiece(tile: String, p: Option[Piece]) = {
//        tiles(tile) = tiles(tile).copy(piece = p)
//    }
//
//    def tryMove(move: Move): Boolean = {
//        tryMove(move.from, move.to)
//    }
//
//    def tryMove(f: Tile, t: Tile): Boolean = {
//        f.piece match {
//            case Some(p) if p.shade != hasTurn => return false
//            case _ => ()
//        }
//        val move: Option[Move] = getLegalMoves(f).find(m => m == (f, t))
//        move match {
//            case None => false
//            case Some(m) => movePiece(m); true
//        }
//    }
//
//    
//
//    def movePiece(move: Move): Unit = {
//        def makeMove(m: Move): Unit = {
//            val piece: Option[Piece] = m.from.piece match {
//                case Some(Pawn(s, id)) if m.to.rank == 1 || m.to.rank == 8 => Some(Queen(s, id)) // for Promotion
//                case Some(p) => Some(p)
//                case _ => None
//            }
//            updateEnPassantOption(m)
//            updatePiecePos(m.to.piece, None)
//            placePiece(m.to, piece)
//            placePiece(m.from, None)
//            updatePiecePos(piece, Some(m.to))
//        }
//
//        move.isCastles match {
//            case Some(m) => makeMove(m)
//            case None => ()
//        }
//        move.isEnPassant match {
//            case Some(t) => placePiece(t, None) 
//            case None => ()
//        }
//        
//        makeMove(move)
//        updateCastlingRights(move)
//        updateVision(move)
//        changeTurn()
//    }
//
//    def updateEnPassantOption(move: Move): Unit = {
//        val pawnMoved2Tiles = move.from.piece match {
//            case Some(p @ Pawn(s, _)) if Math.abs(move.from.rank - move.to.rank) == 2  => true
//            case _ => false
//        }
//        if (!pawnMoved2Tiles) {
//            enPassantMove = None
//            return
//        }
//        //some magic math to figure out the tile between the jump (from + to) / 2 gives rank 3 and 6 respectively
//        val captureName = move.from.file.toString + ((move.from.rank + move.to.rank) / 2).toString
//        val captureTile = getTileUnsafe(captureName)
//        enPassantMove = Some(enPassant(captureTile, move.to))
//    }
//
//    def updateCastlingRights(m: Move): Unit = Seq(m.from.name, m.to.name).foreach{ n => 
//        castlingRights = n match {
//            case "e1" => castlingRights.set(Light, KingSide).set(Light, QueenSide)
//            case "a1" => castlingRights.set(Light, QueenSide)
//            case "h1" => castlingRights.set(Light, KingSide)
//            case "e8" => castlingRights.set(Dark, KingSide).set(Light, QueenSide)
//            case "a8" => castlingRights.set(Dark, QueenSide)
//            case "h8" => castlingRights.set(Dark, KingSide)
//            case _ => castlingRights
//        }
//    }
//    
//    def getTileRect(str: String): Option[Rectangle] = {
//        Some(tiles(str).rect)
//    }
//
//    def getTile(str: String): Option[Tile] = tiles.get(str)
//
//    def getTileUnsafe(str: String): Tile = tiles(str)
//
//    def getTile(l: Char, n: Int): Option[Tile] = {
//        val str = l.toString + n.toString
//        getTile(str)
//    }
//
//    def getTile(mousePostion: Position): Option[Tile] = tiles.values.find(_.isInside(mousePostion))
//    
//    def getRanks(): Seq[Seq[Tile]] = {
//        Seq.from(tiles.values).groupBy(_.name(1)).values.toSeq.map(s => s.sortBy(_.name))
//    }
//
//    def getTiles(): Seq[Tile] = Seq.from(tiles.values).sortBy(_.name).toSeq
//
//    //def getPieces(shade: Shade): Seq[Piece] = getTiles().flatMap(t => t.piece).filter(_.shade == shade)
//    //def getPieces(piece: Piece): Seq[Piece] = getPieces(piece.shade).filter(_ == piece)
//
//     def boardAsString(): String = {var strBuilder = ""
//        val rankList = getRanks().sortBy(s => s(0).name).reverse
//        for (rank <- rankList) {
//            var emptyCounter = 0
//            for (t <- rank) {
//                t.piece match {
//                    case None => emptyCounter += 1
//                    case Some(p) if emptyCounter == 0 => strBuilder += p.name
//                    case Some(p) => {
//                        strBuilder += emptyCounter.toString + p.name
//                        emptyCounter = 0
//                    } 
//                }
//            }
//            if emptyCounter != 0 then strBuilder += emptyCounter
//            strBuilder += "/"
//        }
//        strBuilder = strBuilder.dropRight(1)
//        strBuilder
//    }
//
//    override def toString(): String = {
//        val enPass: String = enPassantMove match {
//            case None => "-"
//            case Some(value) => value.pawnTarget.name
//        }
//        val nextToMove = hasTurn match {
//            case Light => "w"
//            case Dark => "b"
//        }
//
//        Seq[String](boardAsString(), nextToMove, castlingRights.toString, enPass, "0", "1").mkString(" ")
//    }
//
//}
//
//
//object Board {
//    def apply(boardSize: Int, screenDims: Int): Board = {
//        def makeTile(notation: String): Tile = {
//            val r = makeTileRect(notation)
//            val s = getShade(notation)
//            Tile(s, r, notation)
//        }
//        def makeTileRect(notation: String): Rectangle = {
//            val (l, n) = notationStot(notation)
//            val x = ((l - 'a') * tileSize)
//            val y = (boardSize - (n)) * tileSize
//            val pos = Position(x, y)
//            val color = getShade(notation) match {
//                case Dark => Black
//                case Light => White
//            }
//            Rectangle(pos, color, tileSize)
//        }
//        def tileSize = screenDims.toFloat / boardSize
//        val tiles: MMap[String, Tile] = {
//            val pairs = for {
//                l <- 'a' until ('a' + boardSize).toChar
//                n <- 1 to boardSize
//                str = l.toString + n.toString
//            } yield (str, makeTile(str))
//            MMap.from(pairs)
//        }
//        new Board(tiles)
//    }
//
//    def getShade(notation: String): Shade = {
//        val (l, n) = notationStot(notation)
//        if (l.toInt + n) % 2 == 0 then Dark else Light
//    }
//
//    def notationStot(notation: String): (Char, Int) =
//        (notation(0), notation(1).asDigit)
//
//    
//}
//