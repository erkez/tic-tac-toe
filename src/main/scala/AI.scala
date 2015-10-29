import State.Move
import TicTacToe._

trait AI extends GameRunner {
  type Row = Vector[Player]

  /**
   * Returns true if attacker can win with a next move
   * @param attacker Attacker of row
   * @param row Row to assess threaten
   * @return True of threatened
   */
  def isRowThreatened(attacker: Player)(row: Row): Boolean = {
    val attackCount = row count (_==attacker)
    lazy val defenderCount = row count (_==attacker.opponent)
    attackCount == (boardSize - 1) && defenderCount == 0
  }
  
  def hasThreat(attacker: Player)(implicit board: Board): Boolean =
    board.sequences exists isRowThreatened(attacker)

  def hasFork(attacker: Player)(implicit board: Board): Boolean =
    (board.sequences count isRowThreatened(attacker)) > 1

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

    lazy val blockForkingPositions: Stream[Move] =
      if (!hasFork(opponent)) Stream()
      else for {
        move <- availableMoves
        newState = state play move
        if !hasFork(opponent)
      } yield move

    lazy val centerMove: Stream[Move] = board.centerPosition match {
      case None => Stream()
      case Some(p) => if (state.availablePositions contains p) Stream((player, p)) else Stream()
    }

    lazy val oppositeCorners: Stream[Move] = for {
      corner <- board.cornerPositions.toStream
      oppositeCorner = board getOppositePosition corner
      if board(corner) == opponent && board(oppositeCorner) == NoPlayer
    } yield (player, oppositeCorner)

    lazy val emptyCorners: Stream[Move] = for {
      corner <- board.cornerPositions.toStream
      if board(corner) == NoPlayer
    } yield (player, corner)

    lazy val emptySequences: Stream[Move] = ???

    val priority = winningPositions ++ blockingPositions ++ forkingPositions ++
      blockForkingPositions ++ centerMove ++ oppositeCorners ++ emptyCorners ++ availableMoves

    priority.head
  }

  override def startGame: Player = {
    val aiPlayer = O

    def iterate(state: State, nextPlayer: Player): Player = {
      val moveGetter: (State, Player) => Move =
        if (nextPlayer == aiPlayer) findBestMove else {
          println(state.board)
          getHumanMove
        }

      if (state.availablePositions.isEmpty) {
        return NoPlayer
      }

      val newState = state play moveGetter(state, nextPlayer)
      if (newState.isEndGame) nextPlayer else iterate(newState, nextPlayer.opponent)
    }

    iterate(initialState, X)
  }
}
