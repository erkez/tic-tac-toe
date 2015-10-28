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
      val sets = (rows(board) ++ columns(board) ++ diagonals(board)) map Set.apply
      sets exists { s => s.size == 1 && !s.contains(None) }
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
      if (newState.isEndGame) nextPlayer else iterate(newState, nextPlayer.opponent)
    }
    iterate(initialState, X)
  }
}

object TicTacToe {
  type Board = Matrix[Player]
  type Position = (Int, Int) // (Row, Column)
  type Move = (Player, Position)

  trait Player {
    val opponent: Player = None
  }

  object X extends Player {
    override def toString = "X"
    override val opponent = O
  }

  object O extends Player {
    override def toString = "O"
    override val opponent = X
  }

  object None extends Player {
    override def toString = " "
  }
}