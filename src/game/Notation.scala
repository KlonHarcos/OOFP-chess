package game

type Notation = String

extension (i: Notation) {
    def rank: Int = i(1).asDigit
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
            case Some(next) => i +: next.getAllinDir(dir)
            case None => Seq(i)
    }
}

object Notation {
    def apply(file: Char, rank: Int): Notation = {
        file.toString + rank.toString
    }

    val all: Seq[Notation] = for (l <- 'a' to 'h'; n <- 1 to 8) yield Notation(l, n)

    val PreComputedVis: Map[(Notation, Direction), Seq[Notation]] = {
        val pairs = for (n <- Notation.all; d <- Direction.all) yield ((n, d), n.getAllinDir(d))
        Map.from(pairs)
    }
}

