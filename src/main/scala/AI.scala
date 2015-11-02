import State.Move
import TicTacToe._

/**
 * Trait to use with TicTacToe to play against the AI
 */
trait AI extends GameRunner {

  /**
   * Returns true if attacker can win with a next move
   * @param attacker Attacker of row
   * @param row Row to assess threaten
   * @return True of threatened
   */
  def isRowThreatened(attacker: Player)(row: Vector[Cube.Element[Player]]): Boolean = {
    val attackCount = row count (_.value == attacker)
    lazy val defenderCount = row count (_.value == attacker.opponent)
    attackCount == (boardSize - 1) && defenderCount == 0
  }

  /**
   * Verifies if row is blocked by the opponent of `attacker`
   * @param attacker Attacker of row
   * @param row Row to assess blocking
   * @return True if blocked
   */
  def isRowBlocked(attacker: Player)(row: Vector[Cube.Element[Player]]): Boolean =
    row exists (_.value == attacker.opponent)

  /**
   * Verifies if the `attacker` has a threatening sequence in `board`
   * @param attacker Attacker to consider
   * @param board Board to assess threatens
   * @return True if the board has at least one threat
   */
  def hasThreat(attacker: Player)(implicit board: Cube[Player]): Boolean =
    board.sequences exists isRowThreatened(attacker)

  /**
   * Counts the number of threatening sequences in the `board`
   * @param attacker Attacker to consider
   * @param board Board to count threatens
   * @return The number of threatens
   */
  def getThreatCount(attacker: Player)(implicit board: Cube[Player]): Int =
    board.sequences count isRowThreatened(attacker)

  /**
   * Basic wrapper for `getThreatCount` with count greater than 1.
   * @param attacker Attacker to consider
   * @param board Board to assess fork
   * @return True if fork exists
   */
  def hasFork(attacker: Player)(implicit board: Cube[Player]): Boolean =
    getThreatCount(attacker) > 1

  /**
   * Creates a stream of available moves for `player`
   * @param state State to get available moves from
   * @param player Player to consider
   * @return
   */
  def makeMoves(state: State, player: Player): Stream[Move] =
    for (pos <- state.availablePositions.toStream) yield (player, pos)

  /**
   * Main AI method. Responsible to return the best available move. Obvious, huh?
   * @param state State to consider
   * @param player The AI player
   * @return The best possible move
   */
  def findBestMove(state: State, player: Player): Move = {
    val availableMoves = makeMoves(state, player)
    val opponent = player.opponent
    implicit val board = state.board

    lazy val winningPositions: Stream[Move] = for {
      move <- availableMoves
      if (state play move).isEndGame
    } yield move

    lazy val blockingPositions: Stream[Move] =
      if (!hasThreat(opponent)) Stream()
      else for {
        move <- availableMoves
        if !hasThreat(opponent)((state play move).board)
      } yield move

    lazy val forkingPositions: Stream[Move] = for {
      move <- availableMoves
      newState = state play move
      if hasFork(player)
    } yield move

    lazy val reduceDamagePositions: Stream[Move] = {
      val threatCount = getThreatCount(opponent)
      if (threatCount > 1)
        for {
          move <- availableMoves
          newState = state play move
          if getThreatCount(opponent) < threatCount
        } yield move
      else Stream()
    }

    lazy val centerMove: Stream[Move] = board.center match {
      case Some(center) if center.value == NoPlayer => Stream((player, center.position))
      case _ => Stream()
    }

    lazy val oppositeCorners: Stream[Move] = for {
      corner <- board.corners.toStream
      if corner.value == opponent
      oppositePosition = board.getOppositePosition(corner.position)
      if board(oppositePosition).value == NoPlayer
    } yield (player, oppositePosition)

    lazy val emptyCorners: Stream[Move] = for {
      corner <- board.corners.toStream
      if corner.value == NoPlayer
    } yield (player, corner.position)

    lazy val nonBlockedSequences: Stream[Move] = for {
      seq <- board.sequences.toStream
      if !isRowBlocked(player)(seq)
      position <- seq filter (_.value == NoPlayer) map (_.position)
    } yield (player, position)

    val priority = winningPositions ++ blockingPositions ++ forkingPositions ++ reduceDamagePositions ++
      centerMove ++ oppositeCorners ++ emptyCorners ++ nonBlockedSequences ++ availableMoves

    priority.head
  }

  /**
   * Internal method to get turn move
   * @param state State to consider
   * @param player Player to consider
   * @return Returns move for state and player
   */
  protected override def getTurnMove(state: State, player: Player): Move = {
    val aiPlayer = O
    def moveGetter: (State, Player) => Move  =
      if (player == aiPlayer) findBestMove else getHumanMove
    moveGetter(state, player)
  }
}
