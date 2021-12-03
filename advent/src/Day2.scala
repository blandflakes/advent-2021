import cats.effect.{ExitCode, IO, IOApp}
import fs2.io.file.Path
import io.lines

object Day2 extends IOApp {
  case class Position(x: Int, z: Int)
  case class Aimed(position: Position, aim: Int)
  sealed abstract class Instruction(val magnitude: Int) extends Product {
    def day1Op(position: Position): Position
    def day2Op(aimed: Aimed): Aimed
  }
  case class Forward(override val magnitude: Int)
      extends Instruction(magnitude) {
    override def day1Op(position: Position): Position =
      position.copy(x = position.x + magnitude)

    override def day2Op(aimed: Aimed): Aimed = {
      aimed.copy(
        position = aimed.position
          .copy(
            x = aimed.position.x + magnitude,
            z = aimed.position.z + aimed.aim * magnitude
          )
      )
    }
  }
  case class Back(override val magnitude: Int) extends Instruction(magnitude) {
    override def day1Op(position: Position): Position =
      position.copy(x = position.x - magnitude)

    override def day2Op(aimed: Aimed): Aimed =
      aimed.copy(position = day1Op(aimed.position))
  }
  case class Down(override val magnitude: Int) extends Instruction(magnitude) {
    override def day1Op(position: Position): Position =
      position.copy(z = position.z + magnitude)

    override def day2Op(aimed: Aimed): Aimed =
      aimed.copy(aim = aimed.aim + magnitude)
  }

  case class Up(override val magnitude: Int) extends Instruction(magnitude) {
    override def day1Op(position: Position): Position =
      position.copy(z = position.z - magnitude)

    override def day2Op(aimed: Aimed): Aimed =
      aimed.copy(aim = aimed.aim - magnitude)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val commands = lines[IO](Path("inputs/day2.txt"))
      .map { line =>
        val parts = line.split(" ")
        val magnitude = parts(1).toInt
        parts(0) match {
          case "forward" => Forward(magnitude)
          case "back"    => Back(magnitude)
          case "down"    => Down(magnitude)
          case "up"      => Up(magnitude)
        }
      }
    val day1 = commands
      .fold(Position(0, 0)) { case (currentPosition, instruction) =>
        instruction.day1Op(currentPosition)
      }
      .map(p => p.x * p.z)
      .compile
      .last
      .map(println)

    val day2 = commands
      .fold(Aimed(Position(0, 0), aim = 0)) {
        case (currentPosition, instruction) =>
          instruction.day2Op(currentPosition)
      }
      .map(p => p.position.x * p.position.z)
      .compile
      .last
      .map(println)
    for {
      _ <- day1
      _ <- day2
    } yield ExitCode.Success
  }
}
