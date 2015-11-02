import Cube.Position
import State.Move
import TicTacToe.{Player, X, NoPlayer}

trait GameRunner {
  protected val boardSize: Int
  protected val initialState: State = State.initial(boardSize)

  protected def showState(state: State) = state.board.showWithPositions(NoPlayer)

  protected final def getHumanMove(state: State, player: Player): Move = {
    def readPosition: Position = {
      println(s"Player $player, type your position:")
      try {
        val input = io.StdIn.readInt()
        state.board.getPositionFromIndex(input).get
      } catch {
        case nse: NoSuchElementException =>
          println("Position outside valid range!")
          readPosition
        case nfe: NumberFormatException =>
          println("Invalid input!")
          readPosition
      }
    }

    showState(state)
    val position = readPosition
    if (!state.availablePositions.contains(position)) {
      println("Position is invalid. Try again.")
      getHumanMove(state, player)
    } else (player, position)
  }

  protected def getTurnMove(state: State, nextPlayer: Player): Move

  def startGame: Player = {
    def startTurn(state: State, nextPlayer: Player): Player = {
      state.winner match {
        case None =>
          val newState = state play getTurnMove(state, nextPlayer)
          startTurn(newState, nextPlayer.opponent)
        case Some(player) =>
          showState(state)
          player
      }
    }

    startTurn(initialState, X)
  }
}
