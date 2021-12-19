import Day18.SailfishNumber.nextId
import atto.Atto._
import atto._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
object Day18 extends IOApp {

  class SailfishId(val underlying: Int) extends AnyVal {
    override def toString: String = underlying.toString
  }

  object SailfishNumber {
    // Set up some naive id gen so I can store these in maps without colliding on equivalent contents
    private val idGen = new AtomicInteger(0)
    def nextId: SailfishId = new SailfishId(idGen.incrementAndGet())
  }

  sealed abstract class SailfishNumber {
    val id: SailfishId = nextId
    def magnitude: Int
  }
  final case class Regular(value: Int) extends SailfishNumber {
    override def toString: String = value.toString

    override def magnitude: Int = value
  }
  final case class Pair(left: SailfishNumber, right: SailfishNumber) extends SailfishNumber {
    override def toString: String = s"[$left,$right]"

    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  val numParse: Parser[SailfishNumber] = {
    for {
      _ <- char('[')
      left <- int.token.map(Regular) | numParse
      _ <- char(',')
      right <- int.token.map(Regular) | numParse
      _ <- char(']')
    } yield Pair(left, right)
  }

  // Ok note: this needs to be a mapping of reference... or something. Right now if they have the same contents, they'll map.
  def linkParents(root: SailfishNumber, parent: Option[Pair]): Map[SailfishId, Pair] = {
    val thisMapping = parent.fold(Map.empty[SailfishId, Pair])(p => Map(root.id -> p))
    root match {
      case Regular(_) =>
        thisMapping
      case p: Pair =>
        linkParents(p.left, Some(p)) ++ linkParents(p.right, Some(p)) ++ thisMapping
    }
  }

  def leftParentOf(node: SailfishNumber, parentLinks: Map[SailfishId, Pair]): Option[Pair] = {
    parentLinks.get(node.id).flatMap {
      case p if p.right.id == node.id => Some(p)
      case p                          => leftParentOf(p, parentLinks)
    }
  }

  @tailrec
  def rightLeafOf(node: SailfishNumber): Regular = {
    node match {
      case r: Regular => r
      case p: Pair    => rightLeafOf(p.right)
    }
  }

  def firstLeftRegular(
      current: SailfishNumber,
      parentLinks: Map[SailfishId, Pair]
  ): Option[Regular] =
    leftParentOf(current, parentLinks).map(n => rightLeafOf(n.left))

  def rightParentOf(node: SailfishNumber, parentLinks: Map[SailfishId, Pair]): Option[Pair] = {
    parentLinks.get(node.id).flatMap {
      case p if p.left.id == node.id => Some(p)
      case p                         => rightParentOf(p, parentLinks)
    }
  }

  @tailrec
  def leftLeafOf(node: SailfishNumber): Regular = {
    node match {
      case r: Regular => r
      case p: Pair    => leftLeafOf(p.left)
    }
  }

  def firstRightRegular(
      current: SailfishNumber,
      parentLinks: Map[SailfishId, Pair]
  ): Option[Regular] = rightParentOf(current, parentLinks).map(n => leftLeafOf(n.right))

  def firstExplosion(
      root: SailfishNumber,
      parentLinks: Map[SailfishId, Pair],
      nestedDepth: Int = 0
  ): Option[Map[SailfishId, SailfishNumber]] = {
    root match {
      case Regular(_) =>
        None
      // Exploding pairs are always two regular values, so let's just match that exactly
      case Pair(Regular(leftValue), Regular(rightValue)) if nestedDepth == 4 =>
        val leftUpdate = firstLeftRegular(root, parentLinks).map { toUpdate =>
          toUpdate.id -> Regular(toUpdate.value + leftValue)
        }
        val rightUpdate = firstRightRegular(root, parentLinks).map { toUpdate =>
          toUpdate.id -> Regular(toUpdate.value + rightValue)
        }
        Some((leftUpdate :: rightUpdate :: Some(root.id -> Regular(0)) :: Nil).flatten.toMap)
      case Pair(left, right) =>
        firstExplosion(left, parentLinks, nestedDepth + 1).orElse(firstExplosion(right, parentLinks, nestedDepth + 1))
    }
  }

  def firstSplit(
      root: SailfishNumber
  ): Option[Map[SailfishId, SailfishNumber]] = {
    root match {
      case Regular(value) if value >= 10 =>
        val left = value / 2
        val right = math.ceil(value.toDouble / 2).toInt
        Some(Map(root.id -> Pair(Regular(left), Regular(right))))
      case Regular(_) =>
        None
      case Pair(left, right) =>
        firstSplit(left).orElse(firstSplit(right))
    }
  }

  def applyChanges(root: SailfishNumber, changes: Map[SailfishId, SailfishNumber]): SailfishNumber = {
    changes.getOrElse(
      root.id,
      root match {
        case r: Regular        => r
        case Pair(left, right) => Pair(applyChanges(left, changes), applyChanges(right, changes))
      }
    )
  }

  def reduce(sailfishNumber: SailfishNumber): SailfishNumber = {
    var links = linkParents(sailfishNumber, None)
    var changes = firstExplosion(sailfishNumber, links).orElse(firstSplit(sailfishNumber))
    var updated = changes.fold(sailfishNumber)(applyChanges(sailfishNumber, _))
    while (changes.isDefined) {
      links = linkParents(updated, None)
      changes = firstExplosion(updated, links).orElse(firstSplit(updated))
      updated = changes.fold(updated)(applyChanges(updated, _))
    }
    updated
  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      numbers <- io
        .lines[IO](Path("inputs/day18.txt"))
        .map { l =>
          numParse.parseOnly(l).either match {
            case Left(e)               => throw new IllegalArgumentException(s"Parsing failed: $e")
            case Right(sailfishNumber) => sailfishNumber
          }
        }
        .compile
        .toList
      part1 = numbers.reduce[SailfishNumber] { case (left, right) => reduce(Pair(left, right)) }
      _ = println(part1)
      _ = println(part1.magnitude)
      possibleAdditions = numbers.combinations(2).flatMap(_.permutations)
      answers = possibleAdditions.map(pair => reduce(Pair(pair.head, pair(1))))
      magnitudes = answers.map(_.magnitude)
      part2 = magnitudes.max
      _ = println(part2)
    } yield ExitCode.Success
  }
}
