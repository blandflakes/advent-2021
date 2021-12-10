import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

import scala.annotation.tailrec

object Day10 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val pairs = Map(
      '(' -> ')',
      '[' -> ']',
      '{' -> '}',
      '<' -> '>'
    )

    val scores = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    sealed trait ValidationResult
    final case object Valid extends ValidationResult
    final case class Corrupted(expected: Option[Char], found: Char) extends ValidationResult
    final case class Incomplete(remainingStack: List[Char]) extends ValidationResult

    @tailrec
    def validateLine(line: List[Char], stack: List[Char]): ValidationResult = {
      // for each char, if it's a left, add it to stack, if it's a right, compare to stack - if matches, continue with popped, else return some(theThing)
      line match {
        case Nil          => if (stack.isEmpty) Valid else Incomplete(stack)
        case head :: tail =>
          // Opening character
          if (pairs.contains(head)) {
            validateLine(tail, head :: stack)
          }
          // Closing character
          else {
            val lastOpen = pairs.get(stack.head)
            if (lastOpen.contains(head)) {
              validateLine(tail, stack.tail)
            } else {
              Corrupted(lastOpen, head)
            }
          }
      }

    }

    val validatedLines = io
      .lines[IO](Path("inputs/day10.txt"))
      .map(_.toList)
      .map(l => validateLine(l, List.empty))

    val part1 = validatedLines
      .map {
        case Corrupted(_, found) => Some(found)
        case _                   => None
      }
      .map(_.map(scores))
      .map(_.getOrElse(0))
      .reduce(_ + _)
      .compile
      .lastOrError
      .map(println)

    def scoreStack(stack: List[Char]): Long = stack.foldLeft(0L) { case (total, char) =>
      val points = pairs(char) match {
        case ')' => 1L
        case ']' => 2L
        case '}' => 3L
        case '>' => 4L
      }
      total * 5L + points
    }

    val part2 = validatedLines
      .map {
        case Incomplete(stack) => Some(stack)
        case _                 => None
      }
      .map(_.map(scoreStack))
      .compile
      .toList
      .map(_.flatten)
      .map { scoresList =>
        scoresList.sorted(Ordering[Long])(scoresList.length / 2)
      }
      .map(println)
    for {
      _ <- part1
      _ <- part2
    } yield ExitCode.Success
  }
}
