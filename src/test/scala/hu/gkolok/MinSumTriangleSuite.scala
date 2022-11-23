package hu.gkolok

import hu.gkolok.MinSumTriangle.{Node, Path}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MinSumTriangleSuite extends AnyFunSuite with Matchers {

  test("add nodes to empty path list") {
    val result = MinSumTriangle.addNodes(List.empty, List(Node(2)))
    result shouldBe List(Path(2, List(Node(2))))
  }

  test("add nodes to one node Path") {
    val result = MinSumTriangle.addNodes(List(Path(1, List(Node(1)))), List(Node(1), Node(2)))
    result shouldBe List(Path(2, List(Node(1), Node(1))), Path(3, List(Node(2), Node(1))))
  }

  test("add nodes to 2 nodes Path") {
    val input = List(Path(2, List(Node(1), Node(1))), Path(3, List(Node(2), Node(1))))
    val result = MinSumTriangle.addNodes(input, List(Node(1), Node(2), Node(3)))
    val expected = List(
      Path(3, List.fill(3)(Node(1))),
      Path(4, List(2, 1, 1).map(Node.apply)),
      Path(6, Range.inclusive(3, 1, -1).toList.map(Node.apply))
    )
    result shouldBe expected
  }

  test("add many nodes to a Path list") {
    // input is an ordered List of Paths
    val N = 100
    val inputPaths = Range.inclusive(1, N)
      .toList
      .map(n => Path(n, List(Node(n))))
    val nodes = Range.inclusive(1, N + 1).map(Node.apply).toList
    val result = MinSumTriangle.addNodes(inputPaths, nodes)

    // result should contain the first and last Path extended by the first and last Node, because in that case
    // there is no selection
    val leftMostPath = Path(2, List(Node(1), Node(1)))
    val rightMostPath = Path(N + N + 1, List(Node(N + 1), Node(N)))
    result should contain(leftMostPath)
    result should contain(rightMostPath)

    val expected = leftMostPath +: Range.inclusive(2, N + 1).map { n =>
      Path(n + n - 1, List(Node(n), Node(n - 1)))
    }
    result should contain theSameElementsAs expected
  }
}
