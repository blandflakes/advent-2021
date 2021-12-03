import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import io.lines

import scala.annotation.tailrec

object Day3 extends IOApp {

  def updateWithFrequencies(
      counts: Map[Int, (Int, Int)],
      bitArray: Array[Int]
  ): Map[Int, (Int, Int)] = bitArray.iterator.zipWithIndex.foldLeft(counts) {
    case (counts, (bit, index)) =>
      val current = counts(index)
      val updated = bit match {
        case 0 => current.copy(_1 = current._1 + 1)
        case 1 => current.copy(_2 = current._2 + 1)
      }
      counts + (index -> updated)
  }

  def toDecimal(bitArray: Array[Int]): Int =
    Integer.parseInt(bitArray.mkString, 2)

  override def run(args: List[String]): IO[ExitCode] = {

    val part1 = lines[IO](Path("inputs/day3.txt"))
      .map(_.chars().toArray.map(_ - '0'))
      .fold(Map.empty[Int, (Int, Int)].withDefaultValue((0, 0)))(
        updateWithFrequencies
      )
      .map { counts =>
        val gammaWinners = counts
          .map { case (index, (zeros, ones)) =>
            (index, if (zeros > ones) 0 else 1)
          }
          .toArray
          .sortBy(_._1)
          .map(_._2)
        val epsilonWinners = counts
          .map { case (index, (zeros, ones)) =>
            (index, if (zeros <= ones) 0 else 1)
          }
          .toArray
          .sortBy(_._1)
          .map(_._2)
        val gammaDecimal = toDecimal(gammaWinners)
        val epsilonDecimal = toDecimal(epsilonWinners)
        gammaDecimal * epsilonDecimal
      }
      .compile
      .last
      .map(println)

    val part2 = lines[IO](Path("inputs/day3.txt"))
      .map(_.chars().toArray.map(_ - '0'))
      .compile
      .toList
      .map { readings =>
        def mostFrequentDesired(freqs: (Int, Int)): Int = freqs match {
          case (zeros, ones) if ones >= zeros => 1
          case _                              => 0
        }

        def leastFrequentDesired(freqs: (Int, Int)): Int = freqs match {
          case (zeros, ones) if ones >= zeros => 0
          case _                              => 1
        }

        @tailrec
        def lastSatisfying(
            currentPosition: Int,
            remainingCandidates: List[Array[Int]]
        )(desiredCalc: ((Int, Int)) => Int): Array[Int] = {

          val frequencies = remainingCandidates.foldLeft(
            Map.empty[Int, (Int, Int)].withDefaultValue((0, 0))
          )(updateWithFrequencies)

          if (remainingCandidates.size == 1) {
            remainingCandidates.head
          } else {
            val desired = desiredCalc(frequencies(currentPosition))
            lastSatisfying(
              currentPosition + 1,
              remainingCandidates.filter(_(currentPosition) == desired)
            )(desiredCalc)
          }
        }

        val oxygenWinner =
          lastSatisfying(0, readings)(mostFrequentDesired)
        val co2Winner =
          lastSatisfying(0, readings)(leastFrequentDesired)

        (oxygenWinner, co2Winner)
      }
      .map { case (oxygenBits, co2Bits) =>
        val oxygenDecimal = toDecimal(oxygenBits)
        val co2Decimal = toDecimal(co2Bits)
        oxygenDecimal * co2Decimal
      }
      .map(println)

    for {
      _ <- part1
      _ <- part2
    } yield ExitCode.Success
  }
}
