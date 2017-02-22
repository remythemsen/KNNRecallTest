package tools

/**
  * Created by remeeh on 2/19/17.
  */

object DataPoint {

  abstract class DataPoint[A](data:A) {
    def get:A
    def equalTo(other:DataPoint[A]):Boolean
  }

  abstract class NumericDataPoint[A](data:A) extends DataPoint[A](data) {
    def computeDistance(other:NumericDataPoint[A]):Double
    def getId:Int
  }

  implicit class testCaseDataPoint[A](data:TestCase[A]) extends DataPoint[TestCase[A]](data) {
    override def get = data
    override def equalTo(other: DataPoint[TestCase[A]]): Boolean = ???
  }

  implicit class doubleDataPoint(data:(Int, Array[Double])) extends NumericDataPoint[(Int, Array[Double])](data) {
    override def computeDistance(other: NumericDataPoint[(Int, Array[Double])]): Double = {
      Euclidean.measure(data._2, other.get._2)
    }
    override def get: (Int, Array[Double]) = data
    override def equalTo(other: DataPoint[(Int, Array[Double])]): Boolean = {
      if(other.get._1 == data.get._1) true
      else false
    }

    override def getId: Int = data._1
  }

  implicit class floatDataPoint(data:(Int, Array[Float])) extends NumericDataPoint[(Int, Array[Float])](data) {
    override def computeDistance(other: NumericDataPoint[(Int, Array[Float])]): Double = ???
    override def get: (Int, Array[Float]) = data
    override def equalTo(other: DataPoint[(Int, Array[Float])]): Boolean = {
      if(other.get._1 == data.get._1) true
      else false
    }

    override def getId: Int = data._1
  }
}
