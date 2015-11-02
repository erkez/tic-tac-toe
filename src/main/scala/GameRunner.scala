import Cube.Position
import State.Move
import TicTacToe.{Player, NoPlayer}

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
        case nse: NoSuchElementException => {
          println("Position outside valid range!")
          readPosition
        }
        case nfe: NumberFormatException => {
          println("Invalid input!")
          readPosition
        }
      }
    }

    val position = readPosition
    if (!state.availablePositions.contains(position)) {
      showState(state)
      println("Position is invalid. Try again.")
      return getHumanMove(state, player)
    }
    (player, position)
  }

  def startGame: Player
}
