import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

import java.util.regex.Pattern

object Day5 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val segmentSplitPattern = Pattern.quote(" -> ")
    case class Point(x: Int, y: Int)
    case class Segment(start: Point, end: Point) {

      def pointsWithin: List[Point] = {

        def genRange(start: Int, end: Int): Iterator[Int] =
          (start, end) match {
            case (s, e) if s < e => (start to end by 1).iterator
            case (s, e) if s > e => (start to end by -1).iterator
            case (s, _)          => Iterator.continually(s)
          }

        if (start == end) {
          List(start)
        } else {
          val xRange = genRange(start.x, end.x)
          val yRange = genRange(start.y, end.y)
          xRange
            .zip(yRange)
            .map { case (x, y) =>
              Point(x, y)
            }
        }.toList
      }
    }

    val segments = io.lines[IO](Path("inputs/day5.txt")).map { line =>
      val parts = line.split(segmentSplitPattern)
      val startParts = parts(0).split(',').map(_.toInt)
      val endParts = parts(1).split(',').map(_.toInt)
      Segment(
        Point(startParts(0), startParts(1)),
        Point(endParts(0), endParts(1))
      )
    }

    def plotPoints(countsAcc: Map[Point, Int], segment: Segment) =
      segment.pointsWithin.foldLeft(countsAcc) {
        case (counts: Map[Point, Int], point) =>
          counts + (point -> (counts(point) + 1))
      }

    val part1 = segments
      .filter(s => s.start.x == s.end.x || s.start.y == s.end.y)
      .fold(Map.empty[Point, Int].withDefaultValue(0))(plotPoints)
      .map(_.values.count(_ >= 2))
      .map(println)
      .compile
      .drain

    val part2 = segments
      .fold(Map.empty[Point, Int].withDefaultValue(0))(plotPoints)
      .map(_.values.count(_ >= 2))
      .map(println)
      .compile
      .drain

    for {
      _ <- part1
      _ <- part2
    } yield ExitCode.Success
  }
}
