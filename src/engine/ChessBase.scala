package engine

import processing.core._
import engine.graphics.*
import java.io.File

class ChessBase extends PApplet {
    var angle = 0.0f

    val piecePath: String = "src/images"

    def appSize: Int = 500 
    def boardSize: Int = 8

    lazy val pieceImgMap: Map[String, PImage] = {
        val dir: File = new File(piecePath)
        val filesToImport = dir.listFiles()
        val importedFiles = for {
            f <- filesToImport
            name = f.getName().takeWhile(_ != '.')
            img = loadImage(f.getAbsolutePath())
        } yield {
            val size = appSize / boardSize
            img.resize(size , size)
            (name, img)
        }
        importedFiles.toMap
    }
    override def settings(): Unit = {
        size(appSize, appSize)
    }

    override def setup() = {
        val triggerForLazyMap = pieceImgMap
        setupGame()
        smooth()
    }

    def setupGame(): Unit = {

    }

    def fill(color: Color) = super.fill(color.rgba)

    def drawRectangle(r: Rectangle, hasStroke: Boolean = false): Unit = {
        if (!hasStroke) noStroke() else strokeWeight(4)
        fill(r.color)
        rect(r.pos.x, r.pos.y, r.size, r.size)
    }
}