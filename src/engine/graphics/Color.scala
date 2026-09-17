package engine.graphics

abstract class Color(val rgba: Int)
case object  Red   extends Color(0xFFFF0000)
case object  Green extends Color(0xFF00FF00)
case object  Blue  extends Color(0xFF0000FF)
case object  White extends Color(0xFFFFFFFF)
case object  Black extends Color(0xFF808080)
case object  HighlightGreen extends Color(0x8000FF00)
case object LowHighlightGreen extends Color(0x3000FF00)

