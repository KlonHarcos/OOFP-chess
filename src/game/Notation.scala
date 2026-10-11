package game

type Notation = String

extension (i: Notation) {
    def rank: Int = i.drop(1).toInt
    def file: Char =  i(0)
    def isValid: Boolean = (rank >= 1 && rank <= 8) && (file >= 'a' && file <= 'h') 

    def getVisInDir(dir: Direction): Seq[Notation] = {
        Notation.PreComputedVis((i, dir))
    }

    def at(dir: Direction): Option[Notation] = {
        val newN = Notation((file + dir.l).toChar, rank + dir.n)
        if newN.isValid then Some(newN)
        else None
    }

    private def getAllinDir(dir: Direction): Seq[Notation] = at(dir) match {
        case None => Seq.empty[Notation]
        case Some(n) => n.getAllinDirRec(dir)
    }
    private def getAllinDirRec(dir: Direction): Seq[Notation] = at(dir) match {
            case Some(next) => i +: next.getAllinDirRec(dir)
            case None => Seq(i)
    }
}

object Notation {
    def apply(file: Char, rank: Int): Notation = {
        file.toString + rank.toString
    }

    val all: Seq[Notation] = (for (n <- 8 to 1 by -1; l <- 'a' to 'h') yield Notation(l, n))

    val PreComputedVis: Map[(Notation, Direction), Seq[Notation]] = {
        val pairs = for (n <- Notation.all; d <- Direction.all) yield ((n, d), n.getAllinDir(d))
        Map.from(pairs)
    }
}

