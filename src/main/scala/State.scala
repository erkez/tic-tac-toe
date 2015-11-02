import State.Move
import TicTacToe.{Player, X, O, NoPlayer}

case class State(board: Cube[Player], lastMove: Move) {
  val (lastPlayer, lastPosition) = lastMove

  def play(move: Move): State = move match { case (player, position) =>
    if (lastPlayer == player) throw new IllegalArgumentException(s"Player $lastPlayer just played")
    if (board(position).value != NoPlayer) throw new IllegalArgumentException(s"Position $position already filled")
    else State(board.updated(player, position), move)
  }

  val availablePositions: Set[Cube.Position] =
    (for (elem <- board.iterable if elem.value == NoPlayer) yield elem.position).toSet

  lazy val isEndGame: Boolean = isTie || (board.sequences exists { s =>
    val valueSet: Set[Player] = (s map (_.value)).toSet
    valueSet.size == 1 && !valueSet.contains(NoPlayer)
  })

  lazy val isTie: Boolean = board.sequences forall { s =>
    def containsPlayer(player: Player): Boolean = s.exists(_.value == player)
    containsPlayer(X) && containsPlayer(O)
  }

  lazy val winner: Option[Player] =
    if (isTie) Some(NoPlayer)
    else if (isEndGame) Some(lastPlayer)
    else None
}

object State {
  type Move = (TicTacToe.Player, Cube.Position)

  def initial(boardSize: Int): State = State(Cube.fill(boardSize)(NoPlayer), (NoPlayer, (0, 0, 0)))
}
