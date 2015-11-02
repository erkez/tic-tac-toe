class CubeSpec extends UnitSpec {
  "A 2x2x2 cube" should "have 24 sequences" in {
    val cube: Cube[Int] = Cube.fill(2)(0)
    cube.size should equal(2)
    cube.sequences.size should equal(28)
    cube.corners.size should equal(8)
  }

  it should "not have a center" in {
    val cube = Cube.fill(2)(0)
    cube.center should equal(None)
  }

  "A 3x3x3 cube" should "have 49 sequences" in {
    val cube: Cube[Int] = Cube.fill(3)(0)
    cube.size should equal(3)
    cube.sequences.size should equal(49)
    cube.corners.size should equal(8)

  }

  it should "have a center" in {
    val cube = Cube.fill(3)(0)
    cube.center should equal(Some(Cube.Element(0, (2, 2, 2))))
  }

  "A cube" should "create a new cube when updating a position" in {
    val cube1 = Cube.fill(3)(1)
    val cube2 = cube1.updated(5, (2,2,2))
    cube1((2, 2, 2)) should equal(Cube.Element(1, (2,2,2)))
    cube2((2, 2, 2)) should equal(Cube.Element(5, (2,2,2)))
  }
}
