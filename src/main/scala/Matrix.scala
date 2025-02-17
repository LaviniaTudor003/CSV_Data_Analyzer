import scala.util.Try
import scala.io.Source
import scala.annotation.tailrec

type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix = {
    m match {
      case Some(rows) =>
        val transposedData = if (rows.isEmpty) List.empty[List[Double]] else transposeRows(rows)
        new Matrix(Some(transposedData))
      case None =>
        new Matrix(None)
    }
  }

  private def transposeRows(rows: List[List[Double]]): List[List[Double]] = {
    rows.head.indices.map { colIndex =>
      rows.map(row => row(colIndex))
    }.toList
  }

  def map(f: Double => Double): Matrix = {
    m match {
      case Some(rows) =>
        val tRows = rows.foldRight(List.empty[List[Double]]) { (row, accRows) =>
          val tRow = row.foldRight(List.empty[Double]) { (elem, accElems) =>
            f(elem) :: accElems
          }
          tRow :: accRows
        }
        new Matrix(Some(tRows))
      case None =>
        new Matrix(None)
    }
  }

  def *(other: Matrix): Matrix = {
    (m, other.data) match {
      case (Some(a), Some(b)) if Verify(a, b) =>
        val transposedB = other.transpose.data.getOrElse(List.empty[List[Double]])
        val result = Multiply(a, transposedB)
        new Matrix(Some(result))
      case _ =>
        new Matrix(None)
    }
  }

  private def Verify(a: List[List[Double]], b: List[List[Double]]): Boolean = {
    a.headOption.map(_.length).getOrElse(0) == b.length
  }

  private def Multiply(a: List[List[Double]], transposedB: List[List[Double]]): List[List[Double]] = {
    a.map(rowA =>
      transposedB.map(colB =>
        rowA.zip(colB).foldLeft(0.0) { (sum, pair) =>
          sum + pair._1 * pair._2
        }
      )
    )
  }

  def ++(x: Double): Matrix = {
    m match {
      case Some(rows) =>
        val res = rows.map(row =>
          row.foldRight(List(x))((elem, acc) => elem :: acc)
        )
        new Matrix(Some(res))
      case None =>
        new Matrix(None)
    }
  }

  def -(other: Matrix): Matrix = {
    (m, other.data) match {
      case (Some(rowsA), Some(rowsB)) if Compatible(rowsA, rowsB) =>
        val result = (rowsA zip rowsB).map { case (rowA, rowB) =>
          (rowA zip rowB).map { case (elemA, elemB) => elemA - elemB }
        }
        new Matrix(Some(result))
      case _ =>
        new Matrix(None)
    }
  }

  private def Compatible(a: List[List[Double]], b: List[List[Double]]): Boolean = {
    def checkRows(rowsA: List[List[Double]], rowsB: List[List[Double]]): Boolean = {
      (rowsA, rowsB) match {
        case (Nil, Nil) => true
        case (headA :: tailA, headB :: tailB) =>
          if (headA.length == headB.length) checkRows(tailA, tailB)
          else false
        case _ => false
      }
    }
    if (a.length == b.length) checkRows(a, b)
    else false
  }

  def data: Option[Mat] = m
  def height: Option[Int] = m.map(_.size)
  def width: Option[Int] = m.flatMap(_.headOption.map(_.size))
  override def toString: String = {
    m match {
      case Some(mat) => mat.map(_.mkString(" ")).mkString("\n")
      case None => "Empty Matrix"
    }
  }
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))

  def apply(data: Option[Mat]): Matrix = new Matrix(data)

  def apply(dataset: Dataset): Matrix = dataset match{
    case dataset if dataset.getRows.nonEmpty =>
      val lines = dataset.getRows.map(_.map(_.toDouble))
      Matrix(Some(lines))
    case _ =>
      Matrix(None)
  }
}