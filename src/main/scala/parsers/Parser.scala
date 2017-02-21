package parsers

import java.io.File

import distances.{Cosine, Euclidean}
import tools.DataPoint.{DataPoint, NumericDataPoint, doubleDataPoint, floatDataPoint, testCaseDataPoint}
import tools.{DataPoint, TestCase}

import scala.io.Source

object Parser {
  abstract class Parser[A](iterator:Iterator[String]) {
    def hasNext:Boolean = iterator.hasNext
    def next : Option[A] = {
      if(iterator.isEmpty) None
      else Some(getTuple)
    }
    def getTuple:A
  }
  /**
    * Parses file of format:
    *
    * ############ (x49) 00002131
    * 0.1, 0.2, 3.0, ...
    *
    */
  implicit class RawParserDouble(iterator:Iterator[String]) extends Parser[NumericDataPoint[(Int, Array[Double])]](iterator) {
    override def getTuple: NumericDataPoint[(Int, Array[Double])] = {
      new doubleDataPoint(
        iterator.next.substring(49).toInt, for {
          comp <- iterator.next.split(" ")
        } yield comp.toDouble)
    }
  }

  implicit class RawParserFloat(iterator:Iterator[String]) extends Parser[NumericDataPoint[(Int, Array[Float])]](iterator) {
    override def getTuple: NumericDataPoint[(Int, Array[Float])] = {
      new floatDataPoint(
        iterator.next.substring(49).toInt, for {
          comp <- iterator.next.split(" ")
        } yield comp.toFloat)
    }
  }


  /**
    * Parses file of format:
    *
    * 00002131 0.1, 0.2, 3.0, ...
    *
    */
  implicit class ReducedParserDouble(iterator:Iterator[String]) extends Parser[NumericDataPoint[(Int, Array[Double])]](iterator) {
    override def getTuple: NumericDataPoint[(Int, Array[Double])] = {
      new doubleDataPoint({
        val p = iterator.next.split(" ")
        (p.head.toInt, p.tail.map(x => x.toDouble))
      })
    }
  }

  implicit class ReducedParserFloat(iterator:Iterator[String]) extends Parser[NumericDataPoint[(Int, Array[Float])]](iterator) {
    override def getTuple: NumericDataPoint[(Int, Array[Float])] = {
      new floatDataPoint({
        val p = iterator.next.split(" ")
        (p.head.toInt, p.tail.map(x => x.toFloat))
      })
    }
  }

  /**
    * Parses file of format:
    *
    * Dataset Queriesset reduced|raw Knnstructure K Measure NumericType N
    *
    * @param iterator Iterator
    */
  implicit class TestCasesParser[A](iterator:Iterator[String]) extends Parser[TestCase[A]](iterator) {
    override def getTuple = ???
    def getTestCase = {
      val config = iterator.next.split(" ")
      val data = Source.fromFile(new File(config(0))).getLines()
      val queries = Source.fromFile(new File(config(1))).getLines()
      val dataFormat = config(2)
      val knnStructDir = config(3)
      val K = config(4).toInt
      val measure = config(5) match {
        case "Cosine" => Cosine
        case "Euclidean" => Euclidean
      }
      val numType = config(6)
      val dataSetSize = config(7).toInt

      // new Testcase
      val tc = dataFormat match {
        case "raw" => numType match {
          case "Double" => {
            new TestCase[(Int, Array[Double])](
              measure,
              new RawParserDouble(data),
              new RawParserDouble(queries),
              K,
              dataSetSize)
          }
          case "Float" => {
            new TestCase[(Int, Array[Float])](
              measure,
              new RawParserFloat(data),
              new RawParserFloat(queries),
              K,
              dataSetSize)
          }
        }
        case "reduced" => numType match {
          case "Double" => {
            new TestCase[(Int, Array[Double])](
              measure,
              new ReducedParserDouble(data),
              new ReducedParserDouble(queries),
              K,
              dataSetSize)
          }
          case "Float" => {
            new TestCase[(Int, Array[Float])](
              measure,
              new ReducedParserFloat(data),
              new ReducedParserFloat(queries),
              K,
              dataSetSize)
          }
        }
      }
      tc
    }
  }
}
