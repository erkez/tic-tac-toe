import Matrix._
import TicTacToe._

case class Board(matrix: Matrix[Player]) extends Matrix(matrix.elems) {
  require(matrix.isSquare, "Board requires a square matrix")

  lazy val sequences: Vector[Vector[Player]] = rows ++ columns ++ diagonals
  lazy val centerPosition: Option[Position] = if (length % 2 == 1) Some(length/2, length/2) else None
  lazy val cornerPositions: Vector[Position] = for {
    i <- Vector(0, length - 1)
    j <- Vector(0, length - 1)
  } yield (i, j)

  def getOppositePosition(position: Position): Position = position match {
    case (i, j) => (math.abs(i - (length - 1)), math.abs(j - (length - 1)))
  }

  override def updated(player: Player, position: Position) = Board(matrix.updated(player, position))

  private def showWithPositions: Vector[Vector[String]] = matrix.rows.zipWithIndex map { case (row, i) =>
    val padding = (length*length).toString.length
    def pad(s: Any): String = s.toString.padTo(padding, ' ')
    row.zipWithIndex map {
      case (NoPlayer, j) => pad(length * i + j)
      case (player, _) => pad(player.toString)
    }
  }

  override def toString = showWithPositions map (_ mkString "|") mkString "\n"
}

object Board {
  def empty(size: Int): Board = Board(Matrix.fill(size)(NoPlayer))
}
