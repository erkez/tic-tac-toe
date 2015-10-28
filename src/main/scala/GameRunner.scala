import Matrix.Position
import State.Move
import TicTacToe._

trait GameRunner {
  val boardSize: Int
  protected val initialState: State = State.initial(boardSize)

  private def readPosition(player: Player): Position = {
    println(s"Player $player, type your position:")
    try {
      val input = io.StdIn.readInt()
      val row = input / boardSize
      val column = input % boardSize
      (row, column)
    } catch {
      case nfe: NumberFormatException => {
        println("Invalid input!")
        readPosition(player)
      }
    }
  }

  protected def getHumanMove(state: State, player: Player): Move = {
    val position = readPosition(player)
    if (!state.availablePositions.contains(position)) {
      println("Position is invalid. Try again.")
      return getHumanMove(state, player)
    }
    (player, position)
  }

  def startGame: Player
}
