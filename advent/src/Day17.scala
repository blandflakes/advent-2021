import cats.effect.{ExitCode, IO, IOApp}

object Day17 extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {

    final case class Position(x: Int, y: Int)
    final case class Velocity(x: Int, y: Int)
    final case class Probe(position: Position, velocity: Velocity) {
      def step(): Probe = {
        val newPosition = position.copy(x = position.x + velocity.x, y = position.y + velocity.y)
        val newVelocity = velocity.copy(
          x = if (velocity.x > 0) velocity.x - 1 else if (velocity.x < 0) velocity.x + 1 else 0,
          y = velocity.y - 1
        )
        Probe(newPosition, newVelocity)
      }
    }

    // Return the highest Y if successful
    def evaluateVelocity(v: Velocity, targetX: Range, targetY: Range): Option[(Velocity, Int)] = {
      var p = Probe(Position(0, 0), v)
      var highest = p.position.y
      def inArea(position: Position): Boolean = targetX.contains(position.x) && targetY.contains(position.y)
      def pointless(position: Position): Boolean = (targetX.max < position.x) || targetY.min > position.y
      while (!inArea(p.position) && !pointless(p.position)) {
        p = p.step()
        highest = Math.max(highest, p.position.y)
      }
      if (inArea(p.position)) {
        Some((v, highest))
      } else {
        None
      }
    }

    def potentialVelocities[F[_]](targetX: Range, targetY: Range): fs2.Stream[F, Velocity] = {

      val xRange = (targetX.min, targetX.max) match {
        case (min, max) if min >= 0 && max >= 0 => 0 to max
        case (min, max) if min <= 0 && max <= 0 => max to 0
        case (min, max)                         => min to max // anything outside of min or max would skip the box
      }

      fs2.Stream.emits(xRange).flatMap { x =>
        // This max y is arbitrary. I know it's not MAX_INT, but I can't be bothered to deduce the actual bound.
        fs2.Stream.iterate(targetY.min)(_ + 1).takeWhile(_ <= 5000).map(y => Velocity(x, y))
      }
    }

    for {
      targetX <- IO {
        265 to 287
      } // Need an IO to get the right monad for a for-comprehension, not parsing those inputs
      targetY = -103 to -58
      velocities = potentialVelocities[IO](targetX, targetY)
      successful = velocities
        .parEvalMapUnordered(5) { v =>
          IO.delay(evaluateVelocity(v, targetX, targetY))
        }
        .filter(_.isDefined)
        .map(_.get)
      highest <- successful.reduce[(Velocity, Int)] { case (l, r) => List(l, r).maxBy(_._2) }.compile.lastOrError
      unique <- successful.map(_._1).compile.toList.map(_.toSet)
      _ = println(highest)
      _ = println(unique.size)
    } yield ExitCode.Success
  }
}
