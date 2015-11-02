class Cube[T] private (val size: Int, elements: Vector[T]) {
  import Cube._

  private val sizeRange = Vector.range(1, size + 1)

  private def positionToIndex(position: Position): Int =
    (position._1 - 1) * size * size + (position._2 - 1) * size + (position._3 - 1)

  private def indexToPosition(index: Int): Position =
    (((index / (size * size)) % size) + 1, ((index / size) % size) + 1, (index % size) + 1)

  private def reverseIndex(index: Int): Int = (size - index) + 1

  private lazy val nonDiagonals: Set[Vector[Element[T]]] =
    (sizeRange flatMap { depth =>
      sizeRange map { column =>
        sizeRange map { row => this(depth, row, column) }
      }
    }).toSet ++
    (sizeRange flatMap { depth =>
      sizeRange map { row =>
        sizeRange map { column => this(depth, row, column) }
      }
    }).toSet ++
    (sizeRange flatMap { row =>
      sizeRange map { column =>
        sizeRange map { depth => this(depth, row, column) }
      }
    })

  private lazy val normalDiagonals: Set[Vector[Element[T]]] =
    (sizeRange map { depth =>
      sizeRange map { diagonal => this(depth, diagonal, diagonal)}
    }).toSet ++
    (sizeRange map { depth =>
      sizeRange map { diagonal => this(depth, diagonal, reverseIndex(diagonal))}
    }).toSet ++
    (sizeRange map { column =>
      sizeRange map { diagonal => this(diagonal, diagonal, column)}
    }).toSet ++
    (sizeRange map { column =>
      sizeRange map { diagonal => this(reverseIndex(diagonal), diagonal, column)}
    }).toSet ++
    (sizeRange map { row =>
      sizeRange map { diagonal => this(diagonal, row, diagonal)}
    }).toSet ++
    (sizeRange map { row =>
      sizeRange map { diagonal => this(reverseIndex(diagonal), row, diagonal)}
    }).toSet

  private lazy val cubeDiagonals: Set[Vector[Element[T]]] = {
    val ds = (sizeRange map { diagonal => (
      this ((diagonal, diagonal, diagonal)),
      this ((diagonal, diagonal, reverseIndex(diagonal))),
      this ((diagonal, reverseIndex(diagonal), diagonal)),
      this ((reverseIndex(diagonal), diagonal, diagonal)))
    }).unzip4
    Set(ds._1, ds._2, ds._3, ds._4)
  }

  lazy val sequences: Set[Vector[Element[T]]] =
    nonDiagonals ++ normalDiagonals ++ cubeDiagonals

  lazy val center: Option[Element[T]] =
    if (size % 2 == 1) {
      val center = size / 2 + 1
      Some(this(center, center, center))
    } else None

  lazy val corners: Vector[Element[T]] = for {
    i <- Vector(1, size)
    j <- Vector(1, size)
    k <- Vector(1, size)
  } yield this(i, j, k)

  def getPositionFromIndex(index: Int): Option[Position] =
    if (index > 0 && index <= size * size * size) Some(indexToPosition(index - 1))
    else None

  def getOppositePosition(position: Position): Position = position match {
    case (i, j, k) =>
      def opposite(n: Int): Int = math.abs(n - (size - 1))
      (opposite(i), opposite(j), opposite(k))
  }

  def iterable: Iterable[Element[T]] = elements.zipWithIndex map { case (e, ix) =>
    Element(e, indexToPosition(ix))
  }

  def updated(value: T, position: Position): Cube[T] =
    new Cube[T](size, elements.updated(positionToIndex(position), value))

  def apply(position: Position): Element[T] =
    Element(elements(positionToIndex(position)), position)

  def showWithPositions(emptyElement: T) = {
    def split[A](from: Iterable[A], n: Int): Iterable[Iterable[A]] =
      sizeRange.foldLeft(Vector[Iterable[A]]()) { (v, i) =>
        v :+ from.slice(n * (i - 1), n * i)
      }

    def positionOrElement(zip: (T, Int)): String = zip match { case (element, index) =>
      (if (element == emptyElement) index + 1 else element).toString
    }

    def planes = split(elements.zipWithIndex map positionOrElement, size * size)
    def rows[A](plane: Iterable[A]) = split(plane, size)
    val cube = split(planes map rows, size)

    for {
      plane <- cube
      row <- plane
      el <- row
    } println(el mkString " ")
  }

  override def toString = elements.toString()
}

object Cube {
  type Position = (Int, Int, Int) // Depth, Row, Column
  case class Element[T](value: T, position: Position)

  implicit class Unzip4[A,B,C,D](val xs: Vector[(A,B,C,D)]) extends AnyVal {
    def unzip4: (Vector[A], Vector[B], Vector[C], Vector[D]) =
      xs.foldRight[(Vector[A], Vector[B], Vector[C], Vector[D])]((Vector(),Vector(),Vector(),Vector())) { (x, res) =>
      val (a,b,c,d) = x
      (a +: res._1, b +: res._2, c +: res._3, d +: res._4)
    }
  }

  def fill[T](size: Int)(element: T) = new Cube[T](size, Vector.fill(size * size * size)(element))
}