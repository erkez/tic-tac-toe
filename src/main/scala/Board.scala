import Matrix._
import TicTacToe._

case class Board(matrix: Matrix[Player]) extends Matrix(matrix.elems) {
  require(matrix.isSquare, "Board requires a square matrix")

  override def updated(player: Player, position: Position) = Board(matrix.updated(player, position))

  private def showWithPositions = matrix.rows.zipWithIndex map { case (row, i) =>
    row.zipWithIndex map {
      case (NoPlayer, j) => length * i + j
      case (player, _) => player
    }
  }

  override def toString = showWithPositions map (_ mkString "|") mkString "\n"
}

object Board {
  def empty(size: Int): Board = Board(Matrix.fill(size)(NoPlayer))
}
