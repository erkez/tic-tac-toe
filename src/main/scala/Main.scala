object Main extends App {
  val ticTacToe = new TicTacToe
  val winner = ticTacToe.startGame
  println(s"Player $winner won!")
}
