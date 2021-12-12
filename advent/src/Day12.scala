import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path

object Day12 extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    object Cave {
      def apply(input: String): Cave = input match {
        case "start"                     => Start
        case "end"                       => End
        case s if s.toLowerCase == input => SmallCave(input)
        case l                           => LargeCave(l)
      }
    }

    sealed abstract class Cave(val name: String)
    final case object Start extends Cave("start")
    final case object End extends Cave("end")
    final case class SmallCave(override val name: String) extends Cave(name)
    final case class LargeCave(override val name: String) extends Cave(name)

    def printPaths(paths: List[List[Cave]]): Unit = println(
      paths.map(p => p.map(_.name).mkString(",")).sorted.mkString("\n")
    )

    def pathsFrom(
        cave: Cave,
        caves: Map[Cave, Set[Cave]],
        visitations: Map[Cave, Int],
        exhausted: (Cave, Map[Cave, Int]) => Boolean
    ): List[List[Cave]] = {
      val updatedVisitations = cave match {
        case s: SmallCave => visitations + (s -> (visitations.getOrElse(cave, 0) + 1))
        case _            => visitations
      }

      caves(cave).filterNot {
        case _: LargeCave => false
        case Start        => true
        case End          => false
        case c: SmallCave => exhausted(c, updatedVisitations)
      }.toList match {
        case Nil => List(cave :: Nil)
        case moreCaves =>
          moreCaves.flatMap { nextCave =>
            pathsFrom(nextCave, caves, updatedVisitations, exhausted).map { path =>
              cave :: path
            }
          }
      }
    }

    for {
      caves <- io
        .lines[IO](Path("inputs/day12.txt"))
        .map { line =>
          val parts = line.split('-')
          (Cave(parts(0)), Cave(parts(1)))
        }
        .fold(Map.empty[Cave, Set[Cave]].withDefaultValue(Set.empty[Cave])) { case (caves, (from, to)) =>
          caves + (from -> caves(from).incl(to)) + (to -> caves(to).incl(from))
        }
        .map(_.removed(End))
        .compile
        .lastOrError
      part1Paths = pathsFrom(
        Start,
        caves,
        Map.empty[Cave, Int],
        { case (c, visitations) => visitations.getOrElse(c, 0) >= 1 }
      ).filter(_.last == End)
      _ = println(part1Paths.length)

      part2Paths: List[List[Cave]] = pathsFrom(
        Start,
        caves,
        Map.empty[Cave, Int],
        { case (c, visitations) =>
          visitations.getOrElse(c, 0) > 0 && visitations.values.count(_ > 1) >= 1
        }
      )
      validPaths = part2Paths
        .filter(_.last == End)
      _ = println(validPaths.length)
    } yield ExitCode.Success
  }
}
