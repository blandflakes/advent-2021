import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import grids.{ArrayGrid, Grid}

import scala.annotation.tailrec

object Day15 extends IOApp {

  case class NextPosition(coordinates: (Int, Int), costSoFar: Int)

  class ExpandedGrid(wrapped: Array[Array[Int]], numExpansions: Int) extends Grid[Int] {

    private val wrappedWidth = wrapped(0).length

    override def apply(y: Int, x: Int): Int = {
      val actualY = y % wrapped.length
      val actualX = x % wrappedWidth

      val folds = y / wrapped.length + x / wrappedWidth
      val height = folds + wrapped(actualY)(actualX) match {
        case 9     => 9
        case other => other % 9
      }
      height
    }

    override def length: Int = wrapped.length * numExpansions

    override def width: Int = wrappedWidth * numExpansions
  }

  @tailrec
  private def lowestRisk(
      target: (Int, Int),
      openList: List[NextPosition],
      visited: Set[(Int, Int)],
      grid: Grid[Int]
  ): Option[Int] = {
    // Might be better to have some kind of sorted structure here
    openList.filterNot(p => visited(p.coordinates)).sortBy(_.costSoFar) match {
      case Nil => None
      case head :: _ if head.coordinates == target =>
        Some(head.costSoFar)
      case cheapest :: others =>
        val newCandidates =
          grids
            .orthogonalPoints(cheapest.coordinates._1, cheapest.coordinates._2, grid)
            .map(coords => NextPosition(coords, cheapest.costSoFar + grid(coords._1, coords._2)))
        val updatedVisited = visited.incl(cheapest.coordinates)
        val unvisited = (others ++ newCandidates).filterNot(p => updatedVisited(p.coordinates))
        lowestRisk(target, unvisited, updatedVisited, grid)
    }
  }
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      grid <- io
        .lines[IO](Path("inputs/day15.txt"))
        .map(_.toArray.map(_.toInt - '0'))
        .compile
        .toList
        .map(_.toArray)
      part1Target = (grid.length - 1, grid(0).length - 1)
      part1Cost = lowestRisk(part1Target, List(NextPosition((0, 0), 0)), Set.empty, new ArrayGrid(grid))
      _ = println(part1Cost)
      part2Target = (grid.length * 5 - 1, grid(0).length * 5 - 1)
      part2Cost = lowestRisk(
        part2Target,
        List(NextPosition((0, 0), 0)),
        Set.empty,
        new ExpandedGrid(grid, numExpansions = 5)
      )
      _ = println(part2Cost)
    } yield ExitCode.Success
  }
}
