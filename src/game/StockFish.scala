package game

import scala.jdk.CollectionConverters._
import java.io.{File, PrintWriter, BufferedReader}

class StockFish(shade: Shade, elo: Int = 2200) extends Player(shade: Shade) {
    val stockFish = StockfishEngine("src/stockfish/Stockfish/src/stockfish", elo)
    def getMove(state: GameState): Move = {
        val bestMove = stockFish.getBestMove(state.toString())
        println(f"the best move is: ${bestMove}")
        if (bestMove == "(none)") then Move.empty
        else Move.fromString(bestMove)
    }
    
}

class StockfishEngine(val process: Process, val inputStream: PrintWriter, val outputStream: BufferedReader) {

    
    def sendCommand(command: String): Unit = {
      inputStream.write(command + "\n")
      inputStream.flush()
    }

    def getOutputUntil(stopToken: String): String = {
      while (true) {
          val line = outputStream.readLine()
          if line.startsWith(stopToken) then return line
      }
      ""
    }

    def init(): Unit = {
      sendCommand("uci")
      getOutputUntil("uciok")
      pingEngine()
    }

    def pingEngine(): Unit = {
      sendCommand("isready")
      getOutputUntil("readyok")
    }

    def getBestMove(fen: String, movetimeMs: Int = 1000): String = {
      sendCommand(f"position fen $fen")
      sendCommand(f"go movetime $movetimeMs")
      val result = getOutputUntil("bestmove")

      result.split(" ")(1)
    }

    def close(): Unit = {
      sendCommand("quit")
      inputStream.close()
      outputStream.close()
      process.destroy()
    }
}

object StockfishEngine {
    def apply(path: String): StockfishEngine = {
        val commandArgs = List(path)
        val builder = new ProcessBuilder(commandArgs.asJava)
        try {
            val process = builder.start()
            val writer = new java.io.PrintWriter(process.getOutputStream)
            val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
            val engine = new StockfishEngine(process, writer, reader)
            engine.init()
            engine
        } catch {
            case e: Exception => throw Exception(e)
        }
    }

    def apply(path: String, elo: Int): StockfishEngine = {
        val engine = StockfishEngine(path)
        engine.sendCommand("setoption name UCI_LimitStrength value true")
        engine.sendCommand(s"setoption name UCI_Elo value $elo")
        engine
    }
}