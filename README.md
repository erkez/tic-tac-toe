# Scala 3D Tic-Tac-Toe
A simple 3D Tic-Tac-Toe game in Scala with AI.

## Game Rules
The goal is to place a sequence of your marker, either `X` or `O`, without
 being blocked by your opponent. A sequence may exist in form of a column,
 row or diagonal.

The game ends in a draw if there are no available unblocked sequences.

## AI Disclaimer
Tic-Tac-Toe is really a flawed game. In its traditional 3x3 board, one can always
force a draw. In the 3D 3x3x3 version, the starting player can always win.

To keep it at least a little bit interesting, the AI is hard-configured to be the
second player. If you start the game in the center, you can always win :)

## Running
Simply clone the repository and execute `sbt run` locally.
As of now, it has no special UI and is a basic console application.

### Main object
The application object is really simple and I didn't invest time on it.
I just got really disappointed after losing to the AI every time :)
See the AI disclaimer above.

### Game Modes: AI and Versus
There are 2 special traits available: `AI` and `Versus`.
To play versus, you can use `new TicTacToe with Versus`.
To play AI, you can use `new TicTacToe with AI`.