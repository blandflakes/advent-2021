import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

import java.util.regex.Pattern

object Day14 extends IOApp {
  private val ruleSplitPattern = Pattern.quote(" -> ")

  def iterate[F[_]](template: List[Char], rules: Map[String, Char]): List[Char] = {
    val additions = template
      .sliding(2)
      .map { pair => rules(pair.mkString) }

    fs2.Stream.emits(template).interleaveAll(fs2.Stream.emits(additions.toList)).toList
  }

  private def answer(template: List[Char]): Int = {
    val counts = template.groupBy(identity).view.mapValues(_.size)
    val mostCommon = counts.maxBy(_._2)
    val leastCommon = counts.minBy(_._2)
    mostCommon._2 - leastCommon._2
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      nonEmptyLines <- io.lines[IO](Path("inputs/day14.txt")).compile.toList
      template = nonEmptyLines.head.toList
      rules = nonEmptyLines.tail.map { l =>
        val parts = l.split(ruleSplitPattern)
        (parts(0), parts(1).toCharArray()(0))
      }.toMap
      generations = fs2.Stream.iterate[IO, List[Char]](template)(iterate(_, rules))
      part1 = generations.drop(10).head
      _ <- part1.compile.lastOrError.map(answer).map(println)
      part2 = generations.drop(40).head
      _ <- part2.compile.lastOrError.map(answer).map(println)
    } yield ExitCode.Success
  }
}
