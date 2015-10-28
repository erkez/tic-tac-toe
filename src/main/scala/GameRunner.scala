import TicTacToe.{Position, Player}

trait GameRunner {
  def startGame: Player
  def readPosition: Position
}
