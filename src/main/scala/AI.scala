import State.Move
import TicTacToe._

trait AI extends GameRunner {
  def findBestMove(state: State, player: Player): Move = {
    def isImminentThreat(seq: Vector[Player]): Boolean = {
      val opponentCount = seq count (_==player.opponent)
      lazy val currentPlayerCount = seq count (_==player)
      opponentCount == (boardSize - 1) && currentPlayerCount == 0
    }

    def hasImminentThread(board: Board): Boolean = countSequence (board) (isImminentThreat) > 0

    def countSequence(board: Board)(predicate: Vector[Player] => Boolean): Int = {
      val sequences = board.rows ++ board.columns ++ board.diagonals
      sequences count predicate
    }

    def makeMoves(state: State, player: Player) =
      for (pos <- state.availablePositions) yield (player, pos)

    val availableMoves = makeMoves(state, player).toStream

    lazy val winningPositions: Stream[Move] = for {
      move <- availableMoves
      if (state play move).isEndGame
    } yield move

    //if (!winningPositions.isEmpty) println("winningPositions " + winningPositions.head)

    lazy val blockingPositions: Stream[Move] =
      if (!hasImminentThread(state.board)) Stream()
      else for {
        move <- availableMoves
        if !hasImminentThread((state play move).board)
      } yield move

    //if (!blockingPositions.isEmpty) println("blockingPositions " + blockingPositions.head)

    lazy val forkingPositions: Stream[Move] = ???

    lazy val blockForkingPositions: Stream[Move] = for {
      move <- availableMoves
      forkingState = state play move
      endGameCount = (makeMoves(forkingState, player.opponent) map forkingState.play) count (_.isEndGame)
      if endGameCount == 0
    } yield move

    //if (!blockForkingPositions.isEmpty) println("blockForkingPositions " + blockForkingPositions.head)

    lazy val centerMove: Stream[Move] = {
      if (boardSize % 2 == 1) {
        val center = boardSize / 2
        val move = (player, (center, center))
        if (availableMoves contains move) Stream(move) else Stream()
      } else Stream()
    }

    //if (!centerMove.isEmpty) println("centerMove " + centerMove.head)

    //if (!availableMoves.isEmpty) println("availableMoves " + availableMoves.head)

    lazy val oppositeCorner = ???
    lazy val emptyCorner = ???
    lazy val emptySide = ???

    val priority = winningPositions ++ blockingPositions ++
      /*blockForkingPositions*/ centerMove ++ availableMoves

    priority.head
  }

  override def startGame: Player = {
    val aiPlayer = X

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
