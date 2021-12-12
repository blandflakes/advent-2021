import scala.reflect.ClassTag

package object grids {
  def orthogonalPoints[T](rowIndex: Int, colIndex: Int, grid: Array[Array[T]]): IndexedSeq[(Int, Int)] = {
    val horizontal =
      Math.max(rowIndex - 1, 0) to Math.min(rowIndex + 1, grid.length - 1) zip Iterator.continually(colIndex)
    val vertical = Iterator.continually(rowIndex) zip
      (Math.max(colIndex - 1, 0) to Math.min(colIndex + 1, grid(rowIndex).length - 1))

    (horizontal ++ vertical).filterNot { case (x, y) => x == rowIndex && y == colIndex }
  }

  def allNeighborPoints[T](rowIndex: Int, colIndex: Int, grid: Array[Array[T]]): IndexedSeq[(Int, Int)] = {
    (Math.max(rowIndex - 1, 0) to Math.min(rowIndex + 1, grid.length - 1))
      .flatMap { x =>
        val row = grid(x)
        (Math.max(colIndex - 1, 0) to Math.min(colIndex + 1, row.length - 1)).map { y =>
          (x, y)
        }
      }
      .filterNot { case (x, y) => x == rowIndex && y == colIndex }
  }

  def mapGrid[T1, T2: ClassTag](grid: Array[Array[T1]])(f: T1 => T2): Array[Array[T2]] =
    grid.map(l => l.map(f))
}
