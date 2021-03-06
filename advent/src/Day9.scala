import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import grids.{ArrayGrid, Grid, orthogonalPoints}

object Day9 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    def pointsInBasin(nadir: (Int, Int), grid: Grid[Int]): List[(Int, Int)] = {
      val nadirHeight = grid(nadir._1, nadir._2)
      val includedNeighbors = orthogonalPoints(nadir._1, nadir._2, grid).toList.filter { case (y, x) =>
        val height = grid(y, x)
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
        .map(a => new ArrayGrid(a))
      allPoints = grid.wrapped.indices.flatMap(y => grid.wrapped(0).indices.map(x => (y, x)))
      pointsWithNeighbors = allPoints.map(p => (p, orthogonalPoints(p._1, p._2, grid)))
      lowestPoints = pointsWithNeighbors
        .filter { case (p, neighbors) =>
          neighbors.map { case (y, x) => grid(y, x) }.min > grid(p._1, p._2)
        }
        .map(_._1)
      lowestHeights = lowestPoints.map(p => grid(p._1, p._2))
      riskLevels = lowestHeights.map(_ + 1)

      _ = println(riskLevels.sum)

      basins = lowestPoints.map(p => pointsInBasin(p, grid).toSet)
      largest = basins.map(_.size).sorted.takeRight(3)

      _ = println(largest.product)
    } yield ExitCode.Success
  }
}
