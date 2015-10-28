import Matrix._
import TicTacToe._

class TicTacToe(size: Int = 3) extends GameRunner {
  val initialBoard = Vector.fill(size)(Vector.fill(size)(None))
  val initialState = State(initialBoard, (None, (0, 0)))

  case class State(board: Board, lastMove: Move) {
    val lastPlayer = lastMove._1
    val availablePositions = for {
      r <- 0 until size
      c <- 0 until size
      if board(r)(c) == None
    } yield (r, c)

    val isEndGame: Boolean = {
      val rowSets = rows(board) map Set.apply
      val colSets = columns(board) map Set.apply
      val diagonalSets = diagonals(board) map Set.apply
      def checkWinner(s: Set[Player]) = s.size == 1 && !s.contains(None)
      (rowSets ++ colSets ++ diagonalSets) exists checkWinner
    }

    def play(move: Move): State = move match { case (player, (r, c)) =>
      if (lastPlayer == player) throw new IllegalArgumentException(s"Player just played")
      if (board(r)(c) != None) throw new IllegalArgumentException(s"Position ($r,$c) already filled")
      else State(board.updated(r, board(r).updated(c, player)), move)
    }

    def show = board.zipWithIndex map { case (row, i) =>
      row.zipWithIndex map {
        case (None, j) => size * i + j
        case (player, _) => player
      }
    }

    override def toString = show map (_ mkString "|") mkString "\n"
  }

  override def readPosition: Position = {
    println("Please type your position:")
    val input = io.StdIn.readInt()
    val row = input / size
    val column = input % size
    (row, column)
  }

  override def startGame: Player = {
    def iterate(state: State, nextPlayer: Player): Player = {
      if (state.availablePositions.isEmpty) {
        return None
      }
      println(state)
      val position = readPosition
      if (!state.availablePositions.contains(position)) {
        println("Position is invalid")
        return iterate(state, nextPlayer)
      }
      val move = (nextPlayer, position)
      val newState = state.play(move)
      val newPlayer = if (nextPlayer == X) O else X
      if (newState.isEndGame) nextPlayer else iterate(newState, newPlayer)
    }
    iterate(initialState, X)
  }
}

object TicTacToe {
  type Board = Matrix[Player]
  type Position = (Int, Int) // (Row, Column)
  type Move = (Player, Position)

  trait Player

  object X extends Player {
    override def toString = "X"
  }

  object O extends Player {
    override def toString = "O"
  }

  object None extends Player {
    override def toString = " "
  }
}