import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

object Day9 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    def iterateNeighbors[T](rowIndex: Int, colIndex: Int, grid: Array[Array[T]]): IndexedSeq[(Int, Int)] = {
      val horizontal =
        Math.max(rowIndex - 1, 0) to Math.min(rowIndex + 1, grid.length - 1) zip Iterator.continually(colIndex)
      val vertical = Iterator.continually(rowIndex) zip
        (Math.max(colIndex - 1, 0) to Math.min(colIndex + 1, grid(rowIndex).length - 1))

      (horizontal ++ vertical).filterNot { case (x, y) => x == rowIndex && y == colIndex }
    }

    def pointsInBasin(nadir: (Int, Int), grid: Array[Array[Int]]): List[(Int, Int)] = {
      val nadirHeight = grid(nadir._1)(nadir._2)
      val includedNeighbors = iterateNeighbors(nadir._1, nadir._2, grid).toList.filter { case (x, y) =>
        val height = grid(x)(y)
        height > nadirHeight && height < 9
      }
      nadir :: includedNeighbors.flatMap(n => pointsInBasin(n, grid))
    }
    for {
      grid <- io
        .lines[IO](Path("inputs/day9.txt"))
        .map(_.toArray.map(_.toInt - '0'))
        .compile
        .toList
        .map(_.toArray)
      allPoints = grid.indices.flatMap(x => grid(0).indices.map(y => (x, y)))
      pointsWithNeighbors = allPoints.map(p => (p, iterateNeighbors(p._1, p._2, grid)))
      lowestPoints = pointsWithNeighbors
        .filter { case (p, neighbors) =>
          neighbors.map { case (x, y) => grid(x)(y) }.min > grid(p._1)(p._2)
        }
        .map(_._1)
      lowestHeights = lowestPoints.map(p => grid(p._1)(p._2))
      riskLevels = lowestHeights.map(_ + 1)

      _ = println(riskLevels.sum)

      basins = lowestPoints.map(p => pointsInBasin(p, grid).toSet)
      largest = basins.map(_.size).sorted.takeRight(3)

      _ = println(largest.product)
    } yield ExitCode.Success
  }
}
