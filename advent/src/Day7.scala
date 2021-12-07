import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

object Day7 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      startingPositions <- io
        .lines[IO](Path("inputs/day7.txt"))
        .compile
        .lastOrError
        .map(_.split(',').toList.map(_.toInt))
      potentialAlignments = startingPositions.min to startingPositions.max
      constantCosts = potentialAlignments.map { candidatePosition =>
        startingPositions.map(position => Math.abs(position - candidatePosition)).sum
      }
      part1 = constantCosts.min
      _ = println(part1)
      increasingCosts = potentialAlignments.map { candidatePosition =>
        startingPositions.map { position =>
          val distance = Math.abs(position - candidatePosition)
          (1 to distance).sum
        }.sum
      }
      part2 = increasingCosts.min
      _ = println(part2)
    } yield ExitCode.Success
  }
}
