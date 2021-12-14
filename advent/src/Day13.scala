import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

object Day13 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    sealed trait FoldAxis
    final case class Horizontal(atY: Int) extends FoldAxis
    final case class Vertical(atX: Int) extends FoldAxis

    case class Paper(points: Set[(Int, Int)]) {
      def fold(foldAxis: FoldAxis): Paper = {
        val o = foldAxis match {
          case Horizontal(atY) =>
            val (unchanged, changed) = points.partition { case (_, y) => y < atY }
            val newPoints = unchanged ++ changed.map { case (x, y) => (x, 2 * atY - y) }
            Paper(newPoints)
          case Vertical(atX) =>
            val (unchanged, changed) = points.partition { case (x, _) => x < atX }
            val newPoints = unchanged ++ changed.map { case (x, y) => (2 * atX - x, y) }
            Paper(newPoints)
        }
        o
      }
    }

    def printPaper(paper: Paper): IO[Unit] = IO {
      val xLength = paper.points.map(_._1).max
      val yLength = paper.points.map(_._2).max

      for (y <- 0 to yLength) {
        for (x <- 0 to xLength) {
          if (paper.points((x, y))) {
            print("#")
          } else {
            print(".")
          }
        }
        println
      }
    }

    for {
      groupedLines <- io
        .lines[IO](Path("inputs/day13.txt"))
        .compile
        .toList
        .map(_.partition(_.contains("fold along")))
      (foldLines, coordLines) = groupedLines
      coords = coordLines.map { l =>
        val parts = l.split(',')
        (parts(0).toInt, parts(1).toInt)
      }.toSet
      folds = foldLines.map { l =>
        val parts = l.stripPrefix("fold along ").split('=')
        (parts(0), parts(1).toInt) match {
          case ("y", n) => Horizontal(n)
          case ("x", n) => Vertical(n)
        }
      }

      paper = Paper(coords)
      part1 = paper.fold(folds.head).points.size
      _ = println(part1)
      part2 = folds.foldLeft(paper) { case (p, f) => p.fold(f) }
      _ <- printPaper(part2)
    } yield ExitCode.Success
  }
}
