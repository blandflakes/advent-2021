import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

object Day11 extends IOApp {

  case class Octopus(value: Int, position: (Int, Int), neighborPositions: IndexedSeq[(Int, Int)]) {}

  private def advanceOnce(octopi: Array[Array[Octopus]]): (Array[Array[Octopus]], Int) = {

    var increased = grids.mapGrid(octopi) { octopus =>
      octopus.copy(value = octopus.value + 1)
    }

    var flashed = Set.empty[(Int, Int)]
    var newlyFlashing = increased.flatMap(_.filter(_.value > 9)).filterNot(o => flashed(o.position))
    while (newlyFlashing.nonEmpty) {
      val toIncrement =
        newlyFlashing.flatMap(_.neighborPositions).groupBy(identity).view.mapValues(_.length)

      increased = grids.mapGrid(increased) { octopus =>
        if (toIncrement.contains(octopus.position)) {
          octopus.copy(value = octopus.value + toIncrement(octopus.position))
        } else {
          octopus
        }
      }

      flashed = flashed ++ newlyFlashing.map(_.position)
      newlyFlashing = increased.flatMap(_.filter(_.value > 9)).filterNot(o => flashed(o.position))
    }

    val reset = grids.mapGrid(increased) { octopus =>
      if (octopus.value > 9) {
        octopus.copy(value = 0)
      } else {
        octopus
      }
    }

    (reset, flashed.size)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      grid <- io
        .lines[IO](Path("inputs/day11.txt"))
        .map(_.toArray.map(_.toInt - '0'))
        .compile
        .toList
        .map(_.toArray)
      octopi = grid.zipWithIndex.map { case (line, x) =>
        line.zipWithIndex.map { case (value, y) =>
          Octopus(value, (x, y), grids.allNeighborPoints(x, y, grid))
        }
      }
      generationStream = fs2.Stream.iterate((octopi, 0)) { case (o, _) =>
        advanceOnce(o)
      }
      part1 = generationStream.drop(1).take(100).compile.toList.map { case (_, flashes) => flashes }.sum
      _ = println(part1)
      part2 = generationStream.zipWithIndex
        .find { case ((_, flashed), _) =>
          flashed == grid.map(_.length).sum
        }
        .map(_._2)
        .toList
        .head
      _ = println(part2)
    } yield ExitCode.Success

  }
}
