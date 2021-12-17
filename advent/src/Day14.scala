import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import java.util.regex.Pattern

object Day14 extends IOApp {
  private val ruleSplitPattern = Pattern.quote(" -> ")

  private def elaborate(
      freqs: Map[(Char, Char), Long],
      rules: Map[(Char, Char), Char]
  ): Map[(Char, Char), Long] = {
    freqs.toList
      .flatMap { case (pair, currentCount) =>
        val addition = rules(pair)
        val left = (pair._1, addition)
        val right = (addition, pair._2)
        val updated = List(
          (left, currentCount),
          (right, currentCount)
        )
        updated
      }
      .groupMapReduce(_._1)(_._2)(_ + _)
  }

  private def solve(template: List[Char], rules: Map[String, Char], targetIterations: Int): Long = {
    var freqs = template.sliding(2).foldLeft(Map.empty[(Char, Char), Long]) { case (acc, window) =>
      val key = (window.head, window(1))
      acc + (key -> (acc.getOrElse(key, 0L) + 1L))
    }

    for { _ <- 1 to targetIterations } {
      freqs = elaborate(freqs, rules.map { case (k, v) => ((k.toList.head, k.toList(1)), v) })
    }

    // Ok, now that we have frequencies of all PAIRS...
    val characterMapping = freqs.toList
      .flatMap { case ((c1, c2), count) =>
        List(c1 -> count, c2 -> count)
      }
      .groupMapReduce(_._1)(_._2)(_ + _)
      .view
      // Fudge for overlapping
      .mapValues(_ / 2)

    val mostCommon = characterMapping.maxBy(_._2)
    val leastCommon = characterMapping.minBy(_._2)
    // More fudge
    (mostCommon._2 - leastCommon._2) + 1
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      nonEmptyLines <- io.lines[IO](Path("inputs/day14.txt")).compile.toList
      template = nonEmptyLines.head.toList
      rules = nonEmptyLines.tail.map { l =>
        val parts = l.split(ruleSplitPattern)
        (parts(0), parts(1).toCharArray()(0))
      }.toMap
      part1 = solve(template, rules, 10)
      _ = println(part1)
      part2 = solve(template, rules, 40)
      _ = println(part2)

    } yield ExitCode.Success
  }
}
