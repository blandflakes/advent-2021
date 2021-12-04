import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import io.lines

import scala.annotation.tailrec

object Day4 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    def loadLines: IO[List[List[String]]] = lines[IO](
      Path("inputs/day4.txt"),
      includeEmpty = true
    ).compile.toList
      .map(
        _.foldLeft(
          (List.empty[String], List.empty[List[String]])
        ) { case ((currentBatch, batches: List[List[String]]), line) =>
          if (line.isEmpty) {
            (List.empty[String], batches.appended(currentBatch))
          } else {
            (currentBatch.appended(line), batches)
          }
        }
      )
      .map {
        case (lastBatch, batches) if lastBatch.nonEmpty =>
          batches.appended(lastBatch)
        case (_, batches) => batches
      }

    def parseBoard(lines: List[String]): Array[Array[Int]] = {
      lines.map(_.split("\\s").filter(_.nonEmpty).map(_.toInt)).toArray
    }

    case class BingBongBoard(
        rows: List[Set[Int]],
        columns: List[Set[Int]]
    ) {
      def callNumber(num: Int): BingBongBoard = {
        BingBongBoard(rows.map(_.excl(num)), columns.map(_.excl(num)))
      }

      def wins: Boolean = rows.exists(_.isEmpty) || columns.exists(_.isEmpty)

      def score(winningNumber: Int): Int = rows.map(_.sum).sum * winningNumber
    }

    object BingBongBoard {
      def generate(grid: Array[Array[Int]]): BingBongBoard = {
        val rows: List[Set[Int]] = grid.map(_.toSet).toList
        val columns: List[Set[Int]] =
          grid.indices
            .map(x => Iterator.continually(x).zip(grid(x).indices).toList)
            .map { indices =>
              indices.map { case (y, x) => grid(x)(y) }.toSet
            }
            .toList

        BingBongBoard(rows, columns)
      }
    }

    @tailrec
    def winningBoards(
        acc: List[(BingBongBoard, Int)],
        remainingMoves: List[Int],
        boards: List[BingBongBoard]
    ): List[(BingBongBoard, Int)] = {
      remainingMoves match {
        case Nil => acc
        case next :: tail =>
          val updatedBoards = boards.map(_.callNumber(next))
          val (winners, losers) = updatedBoards.partition(_.wins)
          val wins = winners.map((_, next))
          if (losers.isEmpty) {
            acc ++ wins
          } else {
            winningBoards(acc ++ wins, tail, losers)
          }
      }
    }
    // So, bingo algorithms. I can just remove things from possible wins, right?
    // Yes, so what are the sets? Each row, then each column
    for {
      lineBatches <- loadLines
      // First "batch" is actually a single line, hence the head.head
      moves = lineBatches.head.head
        .split(',')
        .map(_.toInt)
        .toList
      boards = lineBatches.tail.map(parseBoard).map(BingBongBoard.generate)
      winners = winningBoards(List.empty, moves, boards)
      winner = winners.headOption
      part1 = winner.map { case (board, number) => board.score(number) }
      _ <- IO(println(part1))
      lastPlace = winners.last
      part2 = lastPlace._1.score(lastPlace._2)
      _ <- IO(println(part2))
    } yield ExitCode.Success
  }
}
