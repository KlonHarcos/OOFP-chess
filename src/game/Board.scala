package game

import engine.graphics.{Rectangle, Position, Black, White}
import game.{Shade, Dark, Light}
import scala.collection.mutable.Map
import scala.compiletime.ops.boolean
import dotty.tools.dotc.semanticdb.SymbolInformation.Property.CASE

sealed trait CastlingSide
case object KingSide extends CastlingSide
case object QueenSide extends CastlingSide

case class Move(from: Tile, val to: Tile, isCastles: Option[Move] = None, isEnPassant: Option[Tile] = None) {
    def ==(f: Tile, t: Tile): Boolean = {
        this.from == f && this.to == t 
    }
}

case class enPassant(pawnTarget: Tile, pawnToRemove: Tile)

case class CastlingRights(
    whiteKing: Boolean = true,
    whiteQueen: Boolean = true,
    blackKing: Boolean = true,
    blackQueen: Boolean = true
) {
    def getMove(shade: Shade, side: CastlingSide, board: Board): Move = {
        val Seq(kf, kt, rf, rt) = ((shade, side) match {
            case (Dark, KingSide) =>  Seq("e8", "g8", "h8", "f8")
            case (Dark, QueenSide) => Seq("e8", "c8", "a8", "d8")
            case (Light, KingSide) => Seq("e1", "g1", "h1", "f1")
            case (Light, QueenSide) => Seq("e1", "c1", "a1", "d1")
            case input => throw new IllegalArgumentException(s"Unexpected input: $input")
        }).map(board.getTileUnsafe(_))
        Move(kf, kt, isCastles = Some(Move(rf, rt)))
    }


    def set(shade: Shade, side: CastlingSide): CastlingRights = {
        (shade, side) match {
            case (Dark, KingSide) => this.copy(blackKing = false)
            case (Dark, QueenSide) => this.copy(blackQueen = false)
            case (Light, KingSide) => this.copy(whiteKing = false)
            case (Light, QueenSide) => this.copy(whiteQueen = false)
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

    def fromString(str: String): CastlingRights = {
        if str == "-" then return CastlingRights(false, false, false, false)
        var right = CastlingRights()
        str.foreach{c => 
            val side = if c.toLower == 'k' then KingSide else QueenSide
            val shade: Shade = if c.isLower then Dark else Light
            right = right.set(shade, side)
        }
        right
    }
}


class Board(val boardSize: Int, screenDims: Int) {

    var hasTurn: Shade = Light
    def changeTurn() = hasTurn = hasTurn.other
    var enPassantMove: Option[enPassant] = None
    var castlingRights = CastlingRights()
    var inCheck: Option[Shade] = None //FIX ME: implement me
    var visionDark = Seq.empty[Tile]
    var visionLight = Seq.empty[Tile]

    def getVision(shade: Shade): Seq[Tile] = shade match {
        case Dark => visionDark
        case Light => visionLight
    }

    def updateVision(): Unit = {
        visionDark = Seq.empty[Tile]
        visionLight = Seq.empty[Tile]

        for (t <- getTiles()) {
            t.piece match {
                case Some(p) if p.shade == Dark => visionDark = visionDark ++ getPossibleTargetTile(t)
                case Some(p) if p.shade == Light => visionLight = visionLight ++ getPossibleTargetTile(t) 
                case _ => ()
            }
        }

        println("dark vision: " + visionDark)
        println("light vision " + visionLight)
    }

    def hasStartingPiece(str: String): Boolean = { //TODO: decide to keep or discard currently unused
        val t = getTile(str)
        t match {
            case Some(Tile(_,_,_,Some(p))) if !p.hasMoved => true
            case _ => false
        }
    }

    def getEnPassantTile(): Option[Tile] = enPassantMove match
        case None => None
        case Some(e) => Some(e.pawnTarget)

    def isUnderAttack(shadeUnderAttack: Shade, tile: Tile): Boolean = getVision(shadeUnderAttack.other).contains(tile)

    def isInCheck(shade: Shade): Boolean = inCheck.contains(shade)

    def getCastleMove(shade: Shade, side: CastlingSide): Option[Move] = {

        if isInCheck(shade) then return None

        if !castlingRights.get(shade, side) then return None

        val freeTiles = (shade, side) match {
            case (Dark, KingSide) => Seq("f8", "g8")
            case (Dark, QueenSide) => Seq("b8", "c8", "d8")
            case (Light, KingSide) => Seq("f1", "g1")
            case (Light, QueenSide) => Seq("b1", "c1", "d1")
            case input => throw new IllegalArgumentException(s"Unexpected input: $input")
        }

        val tilesToCheck = freeTiles.map(getTileUnsafe(_))

        val tilesAreFree = tilesToCheck.forall(_.piece == None)
        if !tilesAreFree then return None

        val tilesAreUnderAttack = tilesToCheck.exists(isUnderAttack(shade, _))
        if tilesAreUnderAttack then return None

        return Some(castlingRights.getMove(shade, side, this))
    }

    def getValidCastles(shade: Shade): Seq[Move] = {
        Seq(getCastleMove(shade, KingSide), getCastleMove(shade, QueenSide)).flatMap(identity)
    }

    def getPossibleTargetTile(tile: Tile): Seq[Tile] = getValidMoves(tile).map(m => m.to)

    def getValidMoves(tile: Tile): Seq[Move] = {
        val currPiece = tile.piece
        
        val possible = tile.piece match {
            case None => Seq.empty[Tile]
            case Some(p) => p.movePattern.flatMap(f  => f(tile)(this))
        }
        val noSelfCapture = possible.filterNot(t => Piece.hasSameColor(t.piece, currPiece))
        
        val castles = currPiece match {
            case Some(King(s)) => getValidCastles(s)
            case _ => Seq.empty[Move]                    
        }

        noSelfCapture.map{to => Move(tile, to)} ++ castles
    }

    def placePiece(tile: Tile, p: Option[Piece]) = {
        tiles(tile.name) = tile.copy(piece = p)
    }

    def placePiece(tile: String, p: Option[Piece]) = {
        tiles(tile) = tiles(tile).copy(piece = p)
    }

    def tryMove(f: Tile, t: Tile): Boolean = {
        f.piece match {
            case Some(p) if p.shade != hasTurn => return false
            case _ => ()
        }
        val move: Option[Move] = getValidMoves(f).find(m => m == (f, t))
        move match {
            case None => false
            case Some(m) => movePiece(m); true
        }
    }

    def movePiece(move: Move): Unit = {
        def makeMove(m: Move): Unit = {
            println("making move")
            placePiece(m.to, m.from.piece)
            placePiece(m.from, None)
            m.to.piece.foreach(_.hasMoved = true)
            m.to.piece.foreach(p => println("i exist"))
        }

        move.isCastles match {
            case Some(m) => makeMove(m)
            case None => ()
        }
        move.isEnPassant match {
            case Some(t) => placePiece(t, None) 
            case None => ()
        }
        
        makeMove(move)
        
        handleEnPassantOption(move.from, move.to)
        handleCastlingRights(move.from, move.to)
        updateVision()
        
        
        changeTurn()
    }

    def handleEnPassantOption(from: Tile, to: Tile): Unit = {
        val (pawnMoved2Tiles, shade) = from.piece match {
            case Some(p @ Pawn(s)) if !p.hasMoved && (from.rank - to.rank) % 2 == 0  => (true, s)
            case _ => (false, None)
        }
        if !pawnMoved2Tiles then enPassantMove = None; return
        
        //some magic math to figure out the tile between the jump (from + to) / 2 gives rank 3 and 6 respectively
        val captureName = from.file.toString + ((from.rank + to.rank) / 2).toString
        val captureTile = getTileUnsafe(captureName)

        enPassantMove = Some(enPassant(captureTile, to))
    }

    def handleCastlingRights(from: Tile, to: Tile): Unit = Seq(from.name, to.name).foreach{ n => 
        castlingRights = n match {
            case "e1" => castlingRights.set(Light, KingSide).set(Light, QueenSide)
            case "a1" => castlingRights.set(Light, QueenSide)
            case "h1" => castlingRights.set(Light, KingSide)
            case "e8" => castlingRights.set(Dark, KingSide).set(Light, QueenSide)
            case "a8" => castlingRights.set(Dark, QueenSide)
            case "h8" => castlingRights.set(Dark, KingSide)
            case _ => castlingRights
        }
    }
    
    def tileSize = screenDims.toFloat / boardSize

    val tiles: Map[String, Tile] = {
        val pairs = for {
            l <- 'a' until ('a' + boardSize).toChar
            n <- 1 to boardSize
            str = l.toString + n.toString
        } yield (str, makeTile(str))
        Map.from(pairs)
    }

    def getTileRect(str: String): Option[Rectangle] = {
        Some(tiles(str).rect)
    }

    def getTile(str: String): Option[Tile] = tiles.get(str)

    def getTileUnsafe(str: String): Tile = tiles(str)

    def getTile(l: Char, n: Int): Option[Tile] = {
        val str = l.toString + n.toString
        getTile(str)
    }

    def getTile(mousePostion: Position): Option[Tile] = tiles.values.find(_.isInside(mousePostion))


    def makeTile(notation: String): Tile = {
        val r = makeTileRect(notation)
        val s = getShade(notation)
        Tile(s, r, notation)
    }

    def makeTileRect(notation: String): Rectangle = {
        val (l, n) = notationStot(notation)
        val x = ((l - 'a') * tileSize)
        val y = (boardSize - (n)) * tileSize
        val pos = Position(x, y)
        val color = getShade(notation) match {
            case Dark => Black
            case Light => White
        }
        Rectangle(pos, color, tileSize)
    }

    def notationStot(notation: String): (Char, Int) =
        (notation(0), notation(1).asDigit)

    def getShade(notation: String): Shade = {
        val (l, n) = notationStot(notation)
        if (l.toInt + n) % 2 == 0 then Dark else Light
    }

    def getRanks(): Seq[Seq[Tile]] = {
        Seq.from(tiles.values).groupBy(_.name(1)).values.toSeq.map(s => s.sortBy(_.name))
    }

    def getTiles(): Seq[Tile] = Seq.from(tiles.values).sortBy(_.name).toSeq

    def getPieces(shade: Shade): Seq[Piece] = getTiles().flatMap(t => t.piece).filter(_.shade == shade)
    def getPieces(piece: Piece): Seq[Piece] = getPieces(piece.shade).filter(_ == piece)

    override def toString(): String = {var strBuilder = ""
        val rankList = getRanks().sortBy(s => s(0).name).reverse
        for (rank <- rankList) {
            var emptyCounter = 0
            for (t <- rank) {
                t.piece match {
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
