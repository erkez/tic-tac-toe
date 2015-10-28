import Matrix._
import State.Move
import TicTacToe._

case class State(board: Board, lastMove: Move) {

  val lastPlayer = lastMove._1

  def play(move: Move): State = move match { case (player, position) =>
    if (lastPlayer == player) throw new IllegalArgumentException(s"Player just played")
    if (board(position) != NoPlayer) throw new IllegalArgumentException(s"Position $position already filled")
    else State(board.updated(player, position), move)
  }

  val availablePositions = for {
    r <- 0 until board.length
    c <- 0 until board.length
    if board(r)(c) == NoPlayer
  } yield (r, c)

  val isEndGame: Boolean = {
    val sets = (board.rows ++ board.columns ++ board.diagonals) map (_.toSet)
    sets exists { s => s.size == 1 && !s.contains(NoPlayer) }
  }

}

object State {
  type Move = (Player, Position)

  def initial(boardSize: Int): State = State(Board.empty(boardSize), (NoPlayer, (0, 0)))
}
