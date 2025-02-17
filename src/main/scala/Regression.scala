import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {

    def scalarMultiply(matrix: Matrix, scalar: Double): Matrix = {
      val tmp = matrix.data match {
        case Some(mat) => Some(mat.map(_.map(_ * scalar)))
        case None => None
      }
      Matrix.apply(tmp)
    }

    val dataset = Dataset(dataset_file)
    val (trainingDataset, testDataset) = dataset.split(test_percentage)

    val trainSet = trainingDataset.selectColumns(attribute_columns)
    val trainValuesSet = trainingDataset.selectColumn(value_column)

    val validationSet = testDataset.selectColumns(attribute_columns)
    val validationValuesSet = testDataset.selectColumn(value_column)

    val trainFeaturesMatrix = Matrix.apply(trainSet)
    val validationFeaturesMatrix = Matrix.apply(validationSet)

    val trainValuesMatrix = Matrix.apply(trainValuesSet)
    val validationValuesMatrix = Matrix.apply(validationValuesSet)

    val trainDataMatrix = trainFeaturesMatrix ++ 1.0
    val validationDataMatrix = validationFeaturesMatrix

    val n: Int = attribute_columns.size

    var predictions = new Matrix(None)

    val WMatrix = new Matrix(Some(List.fill(attribute_columns.size + 1)(List(0.0))))

    @tailrec
    def gradientDescent(currentW: Matrix, stepsRemaining: Int): Matrix = {
      if (stepsRemaining == 0) currentW
      else {
        val predictions = trainDataMatrix * currentW
        val errors = predictions - trainValuesMatrix
        val transposedFeatures = trainDataMatrix.transpose
        val gradient = transposedFeatures * errors
        val m = trainFeaturesMatrix.height.getOrElse(1).toDouble
        val scaledGradient = gradient.map(_ / m)
        val updatedW = currentW - scalarMultiply(scaledGradient, alpha)
        gradientDescent(updatedW, stepsRemaining - 1)
      }
    }

    val finalW = gradientDescent(WMatrix, gradient_descent_steps)
    val validationPredictions = (validationDataMatrix ++ 1.0) * finalW
    val validationErrors = validationPredictions - validationValuesMatrix

    val absoluteErrors = validationErrors.map(Math.abs)
    val meanValidationError = absoluteErrors.data.getOrElse(List.empty[List[Double]])
      .foldLeft(0.0)((acc, row) => acc + row.sum) / validationFeaturesMatrix.height.getOrElse(1)

    (finalW, meanValidationError)
  }

  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}