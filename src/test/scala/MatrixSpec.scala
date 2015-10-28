class MatrixSpec extends UnitSpec {

  "A square matrix" should "have equal size of rows and columns" in {
    val matrix = Matrix.fill(3, 3)(0)
    matrix.size should be (3, 3)
  }

  "A matrix" should "throw assertion error if column sizes (A) mismatch" in {
    val vectors = Vector(Vector(1, 2, 3), Vector(1, 2))
    intercept[AssertionError] {
      new Matrix(vectors)
    }
  }

  it should "throw assertion error if column sizes (B) mismatch" in {
    val vectors = Vector(Vector(1, 2), Vector(1, 2, 3), Vector(1, 2, 3))
    intercept[AssertionError] {
      new Matrix(vectors)
    }
  }

  it should "throw when column is empty" in {
    val vectors = Vector(Vector())
    intercept[AssertionError] {
      new Matrix(vectors)
    }
  }

  it should "created a new matrix with updated value at given position" in {
    val position: Matrix.Position = (1, 1)
    val value = 5
    val matrix = Matrix.fill(3)(0)
    val matrix2 = matrix.updated(value, position)
    matrix should not be matrix2
    matrix(position) should equal(0)
    matrix2(position) should equal(value)
  }

}
