package hu.gkolok

import cats.Order
import cats.implicits._

object MinSumTriangle {

  case class Node(value: Int) extends AnyVal

  case class Path(total: Int, nodes: List[Node]) {
    def add(node: Node): Path = Path(total + node.value, node +: nodes)

    def show: String = s"${nodes.map(_.value.toString).mkString(" + ")} = $total"
  }

  implicit val pathOrder: Order[Path] = (path1, path2) => path1.total.compare(path2.total)

  def addNodes(paths: List[Path], newNodes: List[Node]): List[Path] = (paths.isEmpty, newNodes.size) match {
    case (_, 0) =>
      paths
    case (true, _) =>
      List(Path(newNodes.head.value, newNodes))
    case _ =>
      val nodeTuples = newNodes.zip(newNodes.drop(1))
      val newPathCandidates: List[(Path, Path)] = paths.zip(nodeTuples).map {
        case (path, (leftNode, rightNode)) =>
          (path.add(leftNode), path.add(rightNode))
      }
      selectPaths(newPathCandidates)
  }

  def toNodes(line: String): List[Node] = line.split(' ')
    .flatMap(value => if (value.isEmpty) None else Some(Node(value.toInt)))
    .toList

  def selectPath(one: Path, other: Path): Path = one.min(other)

  def selectPaths(pathCandidates: List[(Path, Path)]): List[Path] = {
    val initialState: (List[Path], Option[Path]) = (List.empty[Path], None)
    val (selectionResult, lastLeft) = pathCandidates.foldRight(initialState) {
      case ((pathLeft, pathRight), (selectedPaths, None)) =>
        (pathRight +: selectedPaths, Some(pathLeft))
      case ((pathLeft, pathRight), (selectedPaths, Some(previousLeft))) =>
        val selectedPath = selectPath(pathRight, previousLeft)
        (selectedPath +: selectedPaths, Some(pathLeft))
    }
    lastLeft.map(_ +: selectionResult).getOrElse(selectionResult)

  }
}
