import scala.reflect.ClassTag

package object grids {
  // Grids are usually parsed such that the "outside" is the y, because I'm too lazy to do any kind of inversion.
  // Returns (y, x)
  def orthogonalPoints[T](y: Int, x: Int, grid: Array[Array[T]]): IndexedSeq[(Int, Int)] = {
    val ys =
      Math.max(y - 1, 0) to Math.min(y + 1, grid.length - 1) zip Iterator.continually(x)
    val xs = Iterator.continually(y) zip
      (Math.max(x - 1, 0) to Math.min(x + 1, grid(y).length - 1))

    (ys ++ xs).filterNot { case (ny, nx) => ny == y && nx == x }
  }

  // Returns (y, x)
  def allNeighborPoints[T](y: Int, x: Int, grid: Array[Array[T]]): IndexedSeq[(Int, Int)] = {
    (Math.max(y - 1, 0) to Math.min(y + 1, grid.length - 1))
      .flatMap { ny =>
        val row = grid(ny)
        (Math.max(x - 1, 0) to Math.min(x + 1, row.length - 1)).map { nx =>
          (ny, nx)
        }
      }
      .filterNot { case (ny, nx) => ny == y && nx == x }
  }

  def mapGrid[T1, T2: ClassTag](grid: Array[Array[T1]])(f: T1 => T2): Array[Array[T2]] =
    grid.map(l => l.map(f))

  def showGrid[T](grid: Array[Array[T]]): String = grid.map(_.mkString).mkString("\n")
}
