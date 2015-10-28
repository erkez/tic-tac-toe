object Matrix {
  type Matrix[T] = Vector[Vector[T]]

  def rows[T](matrix: Matrix[T]): Matrix[T] = matrix
  def columns[T](matrix: Matrix[T]): Matrix[T] = matrix.indices.toVector map { column =>
    for (row <- matrix.indices.toVector) yield matrix(row)(column)
  }
  def diagonals[T](matrix: Matrix[T]): Matrix[T] = {
    val diagonals = matrix.indices map { x =>
      (matrix(x)(x), matrix(x)(math.abs(x - matrix.length) - 1))
    }
    val (d1, d2) = diagonals.toVector.unzip
    Vector(d1, d2)
  }
}
