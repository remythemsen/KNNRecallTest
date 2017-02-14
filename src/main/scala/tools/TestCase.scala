package tools

import distances.Distance
import parsers.Parser

import scala.collection.mutable

/**
  * A Container representing a single testcase to be run
  * @param distance
  * @param data
  */

class TestCase(distance:Distance, numericType:String, data:Parser, queries:Parser, K:Int) { // TODO Extends Runnable
  def getInfo() = {
    (data.fileName, queries.fileName, K.toString, distance.getClass.getName, numericType)
  }
  def run() : Array[Array[(Int, Double)]] = {
    // Get all resultsets
    KNN.search(data, queries, K, distance)
  }
}
