package hu.gkolok

import cats.effect.{IO, IOApp}
import cats.implicits._
import fs2.{Stream, io, text}
import hu.gkolok.MinSumTriangleStream.program

object Main extends IOApp.Simple {

  val stdinSource: Stream[IO, String] = io.stdinUtf8[IO](64 * 1024).through(text.lines)

  def run: IO[Unit] = stdinSource
    .through(program)
    .evalMap(IO.println)
    .compile.drain
}
