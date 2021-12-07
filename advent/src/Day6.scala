import cats.effect.{ExitCode, IO, IOApp}
import fs2.Pure
import fs2.io.file.Path

object Day6 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val fish: fs2.Stream[IO, Map[Int, Long]] = io
      .lines[IO](Path("inputs/day6.txt"))
      .map(
        _.split(',')
          .map(s => s.toInt)
          .toList
          .groupBy(identity)
          .view
          .mapValues(_.size.toLong)
          .toMap
      )

    def cycleFish(fish: Map[Int, Long]): fs2.Stream[Pure, Map[Int, Long]] = fs2.Stream
      .iterate(fish) { fish =>
        fish.foldLeft(Map.empty[Int, Long]) { case (newCounts: Map[Int, Long], (age, count)) =>
          age match {
            case 0 =>
              newCounts + (8 -> count) + (6 -> (newCounts.getOrElse(6, 0L) + count))
            case other =>
              val newAge = other - 1
              newCounts + (newAge -> (newCounts.getOrElse(newAge, 0L) + count))
          }
        }
      }
      // stream includes initial fish, drop first for this part
      .drop(1)

    val part1 = fish
      .flatMap(cycleFish)
      .take(80)
      .compile
      .lastOrError
      .map(_.values.sum)
      .map(println)

    val part2 = fish
      .flatMap(cycleFish)
      .take(256)
      .compile
      .lastOrError
      .map(_.values.sum)
      .map(println)

    for {
      _ <- part1
      _ <- part2
    } yield ExitCode.Success
  }
}
