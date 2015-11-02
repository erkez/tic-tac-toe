import State.Move
import TicTacToe._

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

  def isRowBlocked(attacker: Player)(row: Vector[Cube.Element[Player]]): Boolean =
    row exists (_.value == attacker.opponent)
  
  def hasThreat(attacker: Player)(implicit board: Cube[Player]): Boolean =
    board.sequences exists isRowThreatened(attacker)

  def getThreatCount(attacker: Player)(implicit board: Cube[Player]): Int =
    board.sequences count isRowThreatened(attacker)

  def hasFork(attacker: Player)(implicit board: Cube[Player]): Boolean =
    getThreatCount(attacker) > 1

  def makeMoves(state: State, player: Player): Stream[Move] =
    for (pos <- state.availablePositions.toStream) yield (player, pos)
  
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

  override def startGame: Player = {
    val aiPlayer = O

    def iterate(state: State, nextPlayer: Player): Player = {
      val moveGetter: (State, Player) => Move =
        if (nextPlayer == aiPlayer) findBestMove
        else getHumanMove

      state.winner match {
        case None =>
          val newState = state play moveGetter(state, nextPlayer)
          iterate(newState, nextPlayer.opponent)
        case Some(player) =>
          showState(state)
          player
      }
    }

    iterate(initialState, X)
  }
}
