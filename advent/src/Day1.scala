import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import io.lines

object Day1 extends IOApp {

  private def calculateIncreases(depths: fs2.Stream[IO, Int]) = depths
    .fold((0, Option.empty[Int])) {
      case ((increases, prevDepthOpt), currentDepth) =>
        prevDepthOpt.fold((increases, Some(currentDepth)))(prevDepth =>
          if (currentDepth > prevDepth) {
            (increases + 1, Some(currentDepth))
          } else {
            (increases, Some(currentDepth))
          }
        )
    }
    .map { case (totalIncreases, _) => totalIncreases }
    .compile
    .last

  override def run(args: List[String]): IO[ExitCode] = {
    val numbers = lines[IO](Path("inputs/day1.txt")).map(_.toInt)
    val part1 = calculateIncreases(numbers).map(println)

    val windows = calculateIncreases(numbers.sliding(3).map(_.toList.sum))
    val part2 = windows.map(println)
    for {
      _ <- part1
      _ <- part2
    } yield ExitCode.Success
  }

}
