package tools

import distances.Distance
import parsers.Parser
import parsers.Parser.Parser
import tools.DataPoint.NumericDataPoint

/**
  * A Container representing a single testcase to be run
  * @param distance
  * @param data
  */

class TestCase[A](distance:Distance, data:Parser[NumericDataPoint[A]], queries:Parser[NumericDataPoint[A]], K:Int, dataSetSize:Int) { // TODO Extends Runnable
  def run = KNN.search(data, queries, K, distance, dataSetSize)
}
