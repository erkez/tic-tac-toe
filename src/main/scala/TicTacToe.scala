abstract class TicTacToe(val boardSize: Int = 3) extends GameRunner

object TicTacToe {
  trait Player {
    val opponent: Player = NoPlayer
  }

  object X extends Player {
    override def toString = "X"
    override val opponent = O
  }

  object O extends Player {
    override def toString = "O"
    override val opponent = X
  }

  object NoPlayer extends Player {
    override def toString = " "
  }
}
