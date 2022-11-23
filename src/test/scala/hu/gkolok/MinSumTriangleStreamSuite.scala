package hu.gkolok

import cats.effect.IO
import fs2.{Pipe, Stream, text}
import hu.gkolok.MinSumTriangle.{Node, Path}
import hu.gkolok.MinSumTriangleStream._
import munit.CatsEffectSuite
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class MinSumTriangleStreamSuite extends CatsEffectSuite with Matchers {

  private val smallInput = Stream[IO, String](
    "7",
    "6 3",
    "3 8 5",
    "11 2 10 9"
  )

  test("one line input") {
    expect(Stream[IO, String]("1"), program) {
      _ should contain("Minimal path is: 1 = 1")
    }
  }

  test("empty input") {
    expect(Stream[IO, String](""), program) {
      _ should contain("No path found")
    }
  }

  test("small input triangle") {
    expect(smallInput, minPath) {
      _ should contain(Some(Path(18, List(Node(7), Node(6), Node(3), Node(2)))))
    }
  }

  test("small input triangle string result") {
    expect(smallInput, program) {
      _ should contain("Minimal path is: 7 + 6 + 3 + 2 = 18")
    }
  }

  test("500 lines input") {
    val input: Stream[IO, String] = Stream[IO, String](
      Range.inclusive(1, 500)
        .map(n => List.fill(n)(Random.nextInt(100)).mkString(" ")): _*
    )

    expect(input, program) {
      _.head should startWith("Minimal path is: ")
    }
  }

  test("file input from resources") {
    val input: Stream[IO, String] =
      fs2.io.readClassLoaderResource[IO]("triangle.txt")
        .through(text.utf8.decode)
        .through(text.lines)

    expect(input, program) { pathList =>
      pathList.head should startWith("Minimal path is: ")
      pathList.head should endWith("= 50")
    }
  }

  private def expect[O](input: Stream[IO, String], program: Pipe[IO, String, O])(assert: List[O] => Unit) =
    input
      .through(program)
      .compile.toList.map {
      resultList =>
        resultList.size shouldBe 1
        assert(resultList)
    }

}
