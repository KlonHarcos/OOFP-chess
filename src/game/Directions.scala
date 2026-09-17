package game

abstract class Direction(l: Int, n: Int) {
    def getTileName(str: String): String = {
        val (letter, number) = (str(0), str(1).asDigit)
        (letter + l).toChar.toString + (number + n).toString 
    }
}
case object North extends Direction(0,1)
case object South extends Direction(0, -1)
case object East extends Direction(1, 0)
case object West extends Direction(-1, 0)
case object NorthWest extends Direction(-1, 1)
case object NorthEast extends Direction(1, 1)
case object SouthWest extends Direction(-1, -1)
case object SouthEast extends Direction(1, -1)