import cats.effect.Concurrent
import fs2.io.file.{Files, Path}
import fs2.text

package object io {

  def lines[F[_]: Files: Concurrent](
      path: Path,
      includeEmpty: Boolean = false
  ): fs2.Stream[F, String] = {
    Files[F]
      .readAll(path)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty || includeEmpty)
  }
}
