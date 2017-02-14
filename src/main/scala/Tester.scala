import java.io.{File, FileInputStream, ObjectInputStream}

import parsers.{RawParser, TestCasesParser}
import tools.TestCase
import tools.Out

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Runs testcases, outputs results along with the
  * recall difference from true KNN result.
  *
  * Params: testcases file path
  *         knnstructure path
  */

object Tester extends App {
  // TODO Get input params:
  val knnStructurePath = "data/knnstructure"
  val testCasesPath = "data/testcases"
  val outPath = "out/results.data"

  println("Preparing KNN Recall Test...")
  println("Loading True KNN Structure...")
  val tknn = loadKNNStructure(knnStructurePath)
  println("Loading TestCases...")
  val testCases = {
    val parser = new TestCasesParser(new File(testCasesPath))
    val testCases = new ArrayBuffer[TestCase]
    while(parser.hasNext) {
      testCases+=parser.next
    }
    testCases
  }
  println("Starting Recall Test...")
  val resultSets:ArrayBuffer[(String, String, String, String, String, Double)] = new ArrayBuffer()

  // TODO Implement Std dev
  for(testCase <- testCases) {
    // Progress test 1 out of 4
    val results : Array[Array[(Int,Double)]] = testCase.run()

    var averageRecall = 0.0
    for(result <- results) {
      val sortedResSet = result.sortBy(x => x._2)
      averageRecall+={
        val optimal:Array[(Int, Double)] = this.tknn.get(sortedResSet.head._1).head
        val optDistSum = optimal.map(x => x._2).sum
        val resultDistSum = result.tail.map(x => x._2).sum

        optDistSum/resultDistSum
      }
    }

    val result = averageRecall / results.length

    val info = testCase.getInfo()

    resultSets += Tuple6(info._1, info._2, info._3, info._4, info._5, result)
  }

  // Output class
  Out.writeToFile(outPath, resultSets)

  println("Test Finished.")



  def loadKNNStructure(path:String):mutable.HashMap[Int, Array[(Int, Double)]] = {
    val objReader = new ObjectInputStream(new FileInputStream(path))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int,Array[(Int,Double)]]]
    objReader.close()
    hashMap
  }
}
