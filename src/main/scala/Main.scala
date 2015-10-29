object Main extends App {
  val ticTacToe = new TicTacToe with AI
  val winner = ticTacToe.startGame
  val message =
    if (winner != TicTacToe.NoPlayer) s"Player $winner won!"
    else "Just a tie..."
  println(message)
}
