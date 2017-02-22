package tools

import io.Parser.Parser
import tools.DataPoint.NumericDataPoint

/**
  * A Container representing a single testcase to be run
  * @param distance
  * @param data
  */

class TestCase[A](distance:Distance, data:Parser[NumericDataPoint[A]], queries:Parser[NumericDataPoint[A]], K:Int, dataSetSize:Int, querySetSize:Int) { // TODO Extends Runnable
  def getInfo = {
    (data.getClass.getSimpleName, distance.getClass.getSimpleName, K.toString, dataSetSize.toString, querySetSize.toString)
  }
  def run = KNN.search(data, queries, K, distance, dataSetSize)
}
