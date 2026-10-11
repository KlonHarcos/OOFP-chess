# About the Project
This project is an implementation of the (well known) game, Chess with a fully immutable GameState. That implements the *FEN* standard chess notation, to allow loading of gameStates, and interaction with a chess engine like Stockfish All chess moves are fully working, such as:
- En Passant
- Promotions (only to Queen)
- Castles
I implemented highlights on the UI that show **ONLY** legal moves. My original vision included self built evaluation algorithm that would play against the user, this can be found in *Player.scala* unfortunately due to the nature of the implementation and lack of time, it is not efficient enough to run the algorithms. Therefore I implemented a version, that uses the OpenSource StockFish engine to have a worthy opponent.


# Usage
to run the project: 
- to build gradlew: `./gradlew`
- run the program: `./gradlew run`

to customize the game:
- opponents can be set in *ChessGame.scala* by setting player2 to one of the Player subclassses and setting the parameter to reflect whether white or black is the AI playing:
    - **RandomBot** (chooses a random move)
    - **StaticEvalBot** (only uses static eval on each of the moves possible)
    - **MiniMaxBot** (basic implementation of the MiniMax algorithm)
    - **AlphaBetaBot** (minimax with AlphaBeta pruning)
        (both the minimax and the alphabeta algorithms are slow, so max depth cannot be set to more than 3 or so to get any useful interaction)
    - **None** (by setting it to None both sides moves can be made by the user)
    - **StockFish** (this runs the open source chess engine, stockfish to get the moves, the Elo of stockfish can be set as the second parameter)
- starting position:
when loading the board, any legal *FEN* chess position can be loaded in as a string, some already defined examples can be found in *Board.scala*

# Resources
- The framework from the Snake assignment was referenced to implement the UI.
- The images for the pieces are from the open source project by [GiorgioMegrelli on Github](https://github.com/GiorgioMegrelli/chess.com-boards-and-pieces/tree/51131f1e8eb89d62f7d23ecc023f39a36e39dd24/pieces/classic)
- the MiniMax algorithm and other inspirations from [Sebastian Lague](https://www.youtube.com/watch?v=l-hh51ncgDI)'s series on Chess
- some ideas from the [ChessProgramming Wiki](https://chessprogramming.org/)
- **AI USAGE:**
    - For code related issues, to understand Scala concepts and syntax generative AI was used synchronously with StackOverflow
    - For understanding and implementing the ProcessBuilder for StockFish i took a generic input output implementation generated and modified it to fit with the needs of the Stockfish CLI
