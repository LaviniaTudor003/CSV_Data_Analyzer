import scala.io.Source
import scala.annotation.tailrec

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = {
    data
      .map(_.foldLeft("")((x,y) => s"$x $y"))
      .foldLeft("")(_+"\n"+_)
    data.map(_.mkString(",")).mkString("\n")
  }

  def selectColumn(col: String): Dataset = {
    val columnIndex = getHeader.indexOf(col)
    if (columnIndex != -1) {
      val columnData = getColumnDataRecursive(data.tail, columnIndex, List.empty)
      new Dataset(List(col) :: columnData.map(List(_)))
    } else {
      throw new IllegalArgumentException(s"Column '$col' does not exist.")
    }
  }

  private def getColumnDataRecursive(rows: List[List[String]], index: Int, acc: List[String]): List[String] = {
    rows match {
      case Nil => acc.reverse
      case head :: tail =>
        if (index >= 0 && index < head.length) {
          getColumnDataRecursive(tail, index, head(index) :: acc)
        } else {
          throw new IndexOutOfBoundsException(s"Column index $index is out of bounds.")
        }
    }
  }

  def selectColumns(cols: List[String]): Dataset = {
    val columnsData = cols.map { col =>
      val columnIndex = getHeader.indexOf(col)
      if (columnIndex != -1) {
        col :: data.tail.map(_(columnIndex))
      } else {
        throw new IllegalArgumentException(s"Column '$col' does not exist.")
      }
    }
    val rows = columnsData.head.zipWithIndex.map { case (_, index) =>
      columnsData.flatMap(column => List(column(index)))
    }

    new Dataset(rows)
  }

  def split(percentage: Double): (Dataset, Dataset) = {

    val header = getHeader
    val sortedData = data.tail.sortBy(_.head)

    val interval = Math.ceil(1 / percentage).toInt

    var testData: List[List[String]] = List(header)
    var trainData: List[List[String]] = List(header)

    val (testPrepended, trainPrepended) = sortedData.zipWithIndex.foldLeft((List[List[String]](), List[List[String]]())) {
      case ((testAcc, trainAcc), (row, index)) =>
        if ((index + 1) % interval == 0) (row :: testAcc, trainAcc)
        else (testAcc, row :: trainAcc)
    }

    (new Dataset(header :: trainPrepended.reverse), new Dataset(header :: testPrepended.reverse))
  }

  def size: Int = data.length
  def getRows: List[List[String]] = data.tail
  def getHeader: List[String] = data.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = {
    val lines = Source.fromFile(csv_filename).getLines.toList
    val datas = lines.map(_.split(",").toList)
    new Dataset(datas)
  }
  def apply(ds: List[List[String]]): Dataset = {
    new Dataset(ds)
  }
}
