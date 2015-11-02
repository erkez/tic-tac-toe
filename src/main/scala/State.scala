import State.Move
import TicTacToe.{Player, X, O, NoPlayer}

/**
 * Class to keep the state of the board
 * @param board A cube with players
 * @param lastMove The last played move
 */
case class State(board: Cube[Player], lastMove: Move) {
  val (lastPlayer, lastPosition) = lastMove

  /**
   * Creates a new state after playing `move`
   * @param move A move to play
   * @return New state after move
   */
  def play(move: Move): State = move match { case (player, position) =>
    if (lastPlayer == player) throw new IllegalArgumentException(s"Player $lastPlayer just played")
    else if (board(position).value != NoPlayer) throw new IllegalArgumentException(s"Position $position already filled")
    else State(board.updated(player, position), move)
  }

  /**
   * Set of available positions for state
   */
  val availablePositions: Set[Cube.Position] =
    (for (elem <- board.iterable if elem.value == NoPlayer) yield elem.position).toSet

  /**
   * True if the game is over
   */
  lazy val isEndGame: Boolean = isTie || (board.sequences exists { s =>
    val valueSet: Set[Player] = (s map (_.value)).toSet
    valueSet.size == 1 && !valueSet.contains(NoPlayer)
  })

  /**
   * True if the game ended with a tie
   */
  lazy val isTie: Boolean = board.sequences forall { s =>
    def containsPlayer(player: Player): Boolean = s.exists(_.value == player)
    containsPlayer(X) && containsPlayer(O)
  }

  /**
   * The winner of the game
   */
  lazy val winner: Option[Player] =
    if (isTie) Some(NoPlayer)
    else if (isEndGame) Some(lastPlayer)
    else None
}

object State {
  type Move = (TicTacToe.Player, Cube.Position)

  /**
   * Factory for the initial state
   * @param boardSize Size of the board
   * @return
   */
  def initial(boardSize: Int): State = State(Cube.fill(boardSize)(NoPlayer), (NoPlayer, (0, 0, 0)))
}
