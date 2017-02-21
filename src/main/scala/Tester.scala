import java.io.{File, FileInputStream, ObjectInputStream}

import parsers.Parser.{Parser, TestCasesParser}
import tools.TestCase
import tools.Out

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

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
  val parser = new TestCasesParser(Source.fromFile(new File(testCasesPath)).getLines())
  var testCases = new ArrayBuffer[TestCase[_]]()
  while(parser.hasNext) {
    testCases+=parser.getTestCase
  }


  println("Starting Recall Test...")
  val outPutSets:ArrayBuffer[(String, String, String, String, String, Double)] = new ArrayBuffer()

  // TODO Implement Std dev
  for(testCase <- testCases) {
    // Progress test 1 out of 4
    val resultSets = testCase.run
    resultSets.init.head._1


    var averageRecall = 0.0
    for (resultSet <- resultSets) {
      val sortedResSet = resultSet._2.sortBy(x => x._2)
      averageRecall += {
        val optimal: Array[(Int, Double)] = this.tknn.get(resultSet._1.get._1).get
        val optDistSum = optimal.map(x => x._2).sum
        val resultDistSum = resultSet._2.map(x => x._2).sum

        optDistSum / resultDistSum
      }
    }

    val result = averageRecall / resultSets.length

    outPutSets += Tuple6("test", "test", "test", "test", "test", result)

    // Output class
    Out.writeToFile(outPath, outPutSets)

    println("Test Finished.")


  }

  def loadKNNStructure(path:String):mutable.HashMap[Int, Array[(Int, Double)]] = {
    val objReader = new ObjectInputStream(new FileInputStream(path))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int,Array[(Int,Double)]]]
    objReader.close()
    hashMap
  }
}
