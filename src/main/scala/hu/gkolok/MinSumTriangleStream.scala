package hu.gkolok

import cats.effect.IO
import cats.implicits._
import fs2.Pipe
import hu.gkolok.MinSumTriangle._

object MinSumTriangleStream {
  val minPath: Pipe[IO, String, Option[Path]] = s => s
    .map(toNodes)
    .fold(List.empty[Path])(addNodes)
    .map { paths =>
      if (paths.isEmpty) {
        None
      } else {
        val minimal = paths.min
        Some(minimal.copy(nodes = minimal.nodes.reverse))
      }
    }

  val showPath: Pipe[IO, Option[Path], String] = s => s
    .evalMap { optionPath =>
      IO(optionPath.fold("No path found")(p => s"Minimal path is: ${p.show}"))
    }

  val program: Pipe[IO, String, String] = s => s
    .through(minPath)
    .through(showPath)

}
