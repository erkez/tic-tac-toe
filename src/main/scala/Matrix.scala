class Matrix[T](val elems: Vector[Vector[T]]) {
  val size: (Int, Int) = {
    val rows = elems.length
    val rowLengths = elems map (_.length)
    val columns = rowLengths.head
    assert(rowLengths forall (_==columns), "invalid matrix size")
    assert(rows > 0, "matrix must have at least one row")
    assert(columns > 0, "matrix must have at least one column")
    (rows, columns)
  }

  val length = size._1 max size._2

  lazy val isSquare: Boolean = size._1 == size._2

  lazy val rows = elems

  lazy val columns = elems.indices.toVector map { column =>
    for (row <- elems.indices.toVector) yield elems(row)(column)
  }

  lazy val diagonals = {
    val diagonals = elems.indices map { x =>
      (elems(x)(x), elems(x)(math.abs(x - elems.length) - 1))
    }
    val (d1, d2) = diagonals.toVector.unzip
    Vector(d1, d2)
  }

  def updated(value: T, position: Matrix.Position): Matrix[T] = position match { case (r, c) =>
    new Matrix(elems.updated(r, elems(r).updated(c, value)))
  }

  def apply(position: Matrix.Position): T = position match { case (r, c) => elems(r)(c) }
  def apply(row: Int)(column: Int): T = elems(row)(column)
}

object Matrix {
  type Position = (Int, Int) // (Row, Column)

  def fill[T](rows: Int, columns: Int)(v: T): Matrix[T] =
    new Matrix(Vector.fill(rows)(Vector.fill(columns)(v)))

  def fill[T](n: Int)(v: T): Matrix[T] = this.fill(n, n)(v)
}
