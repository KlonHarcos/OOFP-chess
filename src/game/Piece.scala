package game

import dotty.tools.dotc.util.optional

//type Movement = Notation => Set[Seq[Notation]]

type PieceId = Int

trait Sliding

abstract class Piece() {
    val shade: Shade
    val baseName: String
    val id: PieceId
    val value: Int
    def movePattern(n: Notation): Set[Seq[Notation]]

    def name: String = shade match {
        case Dark => baseName.toLowerCase()
        case Light => baseName.toUpperCase()
    }

    def hasSameShade(piece: Option[Piece]): Boolean = piece match {
        case Some(p) if p.shade == this.shade => true 
        case None => false
        case _ => false
    }
}
case class King(shade: Shade, id: PieceId = -1) extends Piece {
    val baseName = "k"
    val value = 100
    def movePattern(n: Notation):  Set[Seq[Notation]] = Direction.all.map(d => Seq(n.at(d)).flatten)
}
case class Queen(shade: Shade, id: PieceId = -1) extends Piece {
    val baseName = "q"
    val value = 9
    def movePattern(n: Notation): Set[Seq[Notation]] = Direction.all.map(d => n.getVisInDir(d))
}
case class Rook(shade: Shade, id: PieceId = -1) extends Piece with Sliding {
    val baseName = "r"
    val value = 5
    def movePattern(n: Notation): Set[Seq[Notation]] = Direction.rook.map(d => n.getVisInDir(d))
}
case class Knight(shade: Shade, id: PieceId = -1) extends Piece {
    val baseName = "n"
    val value = 3
    def movePattern(n: Notation): Set[Seq[Notation]] = Direction.knight.map(d => Seq(n.at(d)).flatten)
}
case class Bishop(shade: Shade, id: PieceId = -1) extends Piece with Sliding {
    val baseName = "b"
    val value = 3
    def movePattern(n: Notation): Set[Seq[Notation]] = Direction.bishop.map(d => n.getVisInDir(d))

}
case class Pawn(shade: Shade, id: PieceId = -1) extends Piece {
    val baseName = "p"
    val value = 1
    def movePattern(n: Notation): Set[Seq[Notation]] = {
        Set.empty[Seq[Notation]] //FIXME IMPLEMENT
    }

    def startingRank = shade match {
        case Dark => 7
        case Light => 2
    }

}

object Piece {
    def createPiecebyName(name: Char, id: PieceId): Piece = {
        val shade: Shade = if (name.isUpper) Light else Dark
        val pType: (Shade, PieceId) => Piece = name.toLower match {
            case 'k' => King.apply
            case 'q' => Queen.apply
            case 'r' => Rook.apply
            case 'n' => Knight.apply
            case 'b' => Bishop.apply
            case 'p' => Pawn.apply
        }
        pType(shade, id)

    }

    def hasSameColor(a: Option[Piece], b: Option[Piece]) = {
        (a, b) match {
            case (Some(p), Some(p2)) if p.shade == p2.shade => true
            case _ => false
        }
    }

    val MaxRange = 8
}

