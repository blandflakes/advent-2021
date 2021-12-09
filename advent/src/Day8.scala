import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

import java.util.regex.Pattern

object Day8 extends IOApp {

  private val segmentSplitPattern = Pattern.quote(" | ")

  override def run(args: List[String]): IO[ExitCode] = {

    case class Entry(inputs: List[String], outputs: List[String])

    val entries = io.lines[IO](Path("inputs/day8.txt")).map { line =>
      val parts = line.split(segmentSplitPattern)
      Entry(parts(0).split(' ').toList, parts(1).split(' ').toList)
    }

    val uniqueLengths = Set(2, 4, 3, 7)

    val part1 =
      entries
        .map(_.outputs.map(_.length))
        .map(_.count(uniqueLengths.contains))
        .reduce(_ + _)
        .compile
        .lastOrError
        .map(println)

    def solve(entry: Entry): Int = {
      val inputSets = entry.inputs.map(_.toSet)

      val one = inputSets.find(_.size == 2).get
      val four = inputSets.find(_.size == 4).get
      val seven = inputSets.find(_.size == 3).get
      val eight = inputSets.find(_.size == 7).get

      val nine = inputSets.find { input =>
        input.size == 6 && four.subsetOf(input)
      }.get

      val zero = inputSets.find { input =>
        input.size == 6 && input != nine && one.subsetOf(input)
      }.get

      val six = inputSets.find { input =>
        input.size == 6 && input != nine && input != zero
      }.get

      val two = inputSets.find { input =>
        input.size == 5 && !input.subsetOf(nine)
      }.get

      val five = inputSets.find { input =>
        input.size == 5 && input.subsetOf(six) & input != two
      }.get

      val three = inputSets.find { input =>
        input.size == 5 && input != five && input != two
      }.get

      val mapping = Map(
        zero -> 0,
        one -> 1,
        two -> 2,
        three -> 3,
        four -> 4,
        five -> 5,
        six -> 6,
        seven -> 7,
        eight -> 8,
        nine -> 9
      )

      entry.outputs.map(_.toSet).map(mapping).mkString.toInt
    }

    val part2 = entries
      .map(solve)
      .reduce(_ + _)
      .compile
      .lastOrError
      .map(println)

    for {
      _ <- part1
      _ <- part2
    } yield ExitCode.Success
  }
}
