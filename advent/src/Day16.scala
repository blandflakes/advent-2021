import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

import scala.collection.mutable.ListBuffer

object Day16 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    sealed abstract class Packet(val version: Long, val `type`: Int) {
      def value: Long
    }
    final case class Literal(override val version: Long, override val value: Long) extends Packet(version, 4)
    final case class Operator(override val version: Long, override val `type`: Int, subPackets: List[Packet])
        extends Packet(version, `type`) {
      override def value: Long = `type` match {
        case 0 => subPackets.map(_.value).sum
        case 1 => subPackets.map(_.value).product
        case 2 => subPackets.map(_.value).min
        case 3 => subPackets.map(_.value).max
        case 5 => if (subPackets.head.value > subPackets(1).value) 1 else 0
        case 6 => if (subPackets.head.value < subPackets(1).value) 1 else 0
        case 7 => if (subPackets.head.value == subPackets(1).value) 1 else 0
      }
    }

    def toDecimal[T](bitArray: List[T]): Long = {
      java.lang.Long.parseLong(bitArray.mkString, 2)
    }

    def parseLiteral(bits: List[Char], version: Long): (Literal, List[Char]) = {
      val words = fs2.Stream.emits(bits).sliding(5, 5)
      val included = words.takeThrough(_.toList.head == '1').toList.flatMap(_.toList.tail)
      val remainder = words.dropThrough(_.toList.head == '1').toList.flatMap(_.toList)
      (Literal(version, toDecimal(included)), remainder)
    }

    def parsePacket(bits: List[Char]): (Packet, List[Char]) = {
      val header = bits.slice(0, 6)
      val version = toDecimal(header.slice(0, 3))
      val `type` = toDecimal(header.slice(3, 6)).toInt
      `type` match {
        case 4 =>
          parseLiteral(bits.slice(6, bits.length), version)
        case o =>
          bits(6) match {
            case '0' =>
              val subPacketsLength = toDecimal(bits.slice(7, 7 + 15)).toInt
              val subPackets = ListBuffer[Packet]()

              var remaining = bits.drop(7 + 15)
              val initialLength = remaining.length
              while (initialLength - remaining.length < subPacketsLength) {
                val (parsedPacket, nextRemaining) = parsePacket(remaining)
                subPackets.addOne(parsedPacket)
                remaining = nextRemaining
              }
              (Operator(version, o, subPackets.toList), remaining)
            case '1' =>
              val numSubpackets = toDecimal(bits.slice(7, 7 + 11)).toInt
              val subPackets = ListBuffer[Packet]()
              var subPacketsBits = bits.slice(7 + 11, bits.length)

              for (_ <- 1 to numSubpackets) {
                val (parsedPacket, remaining) = parsePacket(subPacketsBits)
                subPackets.addOne(parsedPacket)
                subPacketsBits = remaining
              }
              (Operator(version, o, subPackets.toList), subPacketsBits)
          }
      }
    }

    def sumVersions(packet: Packet): Long = packet match {
      case Literal(v, _)              => v
      case Operator(v, _, subPackets) => v + subPackets.map(sumVersions).sum
    }

    for {
      hexMessage <- io.lines[IO](Path("inputs/day16.txt")).map(_.toList).compile.lastOrError
      bitString = hexMessage
        .map {
          case '0' => "0000"
          case '1' => "0001"
          case '2' => "0010"
          case '3' => "0011"
          case '4' => "0100"
          case '5' => "0101"
          case '6' => "0110"
          case '7' => "0111"
          case '8' => "1000"
          case '9' => "1001"
          case 'A' => "1010"
          case 'B' => "1011"
          case 'C' => "1100"
          case 'D' => "1101"
          case 'E' => "1110"
          case 'F' => "1111"
        }
        .flatMap(_.toList)
      (packet, _) = parsePacket(bitString)
      part1 = sumVersions(packet)
      _ = println(part1)
      part2 = packet.value
      _ = println(part2)
    } yield ExitCode.Success
  }
}
