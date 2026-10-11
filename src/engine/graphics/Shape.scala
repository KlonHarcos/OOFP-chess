package engine.graphics

import processing.core._


abstract class Shape(pos: Position, color: Color)
case class Rectangle(pos: Position, color: Color, size: Float) extends Shape(pos: Position, color: Color)