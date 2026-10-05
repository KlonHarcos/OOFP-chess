package game

abstract class Direction(val l: Int, val n: Int) 

case object North extends Direction(0,1)
case object South extends Direction(0, -1)
case object East extends Direction(1, 0)
case object West extends Direction(-1, 0)
case object NorthWest extends Direction(-1, 1)
case object NorthEast extends Direction(1, 1)
case object SouthWest extends Direction(-1, -1)
case object SouthEast extends Direction(1, -1)
case class Jump(val lOffset: Int, val nOffset: Int) extends Direction(lOffset, nOffset)

object Direction {
    val all: Set[Direction] = Set(North, South, East, West, NorthWest, NorthEast, SouthEast, SouthWest)
    val rook: Set[Direction] = Set(North, South, East, West)
    val bishop: Set[Direction] = Set(NorthWest, NorthEast, SouthEast, SouthWest)
    val knight: Set[Direction] = Set(
            Jump(1,2)  , Jump(2,1),
            Jump(-1,-2), Jump(-2,-1),
            Jump(-1,2) , Jump(2,-1),
            Jump(1,-2) , Jump(-2,1)
        )
}