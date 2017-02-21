import java.io._

import distances.{Distance, Euclidean}
import parsers.Parser.{Parser, RawParserDouble, TestCasesParser}
import tools.DataPoint.NumericDataPoint
import tools.{KNN, Out, TestCase}

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
  val data = "data/descriptors-40000.data"
  val queries = "data/queries-10.data"
  val K = 30
  val measure = Euclidean
  val N = Source.fromFile(new File(data)).getLines().length / 2
  val knnStructurePath = "data/"
  val testCasesPath = "data/testcases"
  val outPath = "out/results.data"
  var tknn: mutable.HashMap[Int, Array[(Int, Double)]] = _

  println("Preparing KNN Recall Test...")
  println("Loading True KNN Structure...")
  try {
    tknn = loadKNNStructure(knnStructurePath+"knnstructure")
  } catch {
    case e => {
      println(e)
      println("No usable optimal result structure was found!")
      // With side effect of saving the new structure to disk
      tknn = buildKNNStructure(knnStructurePath, new RawParserDouble(Source.fromFile(new File(data)).getLines()), new RawParserDouble(Source.fromFile(new File(queries)).getLines()), K, measure, N )
    }
  }

  println("Loading TestCases...")
  val parser = new TestCasesParser(Source.fromFile(new File(testCasesPath)).getLines())
  var testCases = new ArrayBuffer[TestCase[(Int, Array[Double])]]()
  while(parser.hasNext) {
    testCases+=parser.getTestCase
  }


  println("Starting Recall Test...")
  val outPutSets:ArrayBuffer[(String, String, String, String, String, Double)] = new ArrayBuffer()

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
    val info = testCase.getInfo

    outPutSets += Tuple6(info._1, info._2, info._3, info._4, info._5, result)

    // Output class
    Out.writeToFile(outPath, outPutSets)

    println("Test Finished.")


  }

  def loadKNNStructure(path:String):mutable.HashMap[Int, Array[(Int, Double)]] = {
    println("Loading Optimal KNN structure")
    val objReader = new ObjectInputStream(new FileInputStream(path))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int,Array[(Int,Double)]]]
    objReader.close()
    hashMap
  }

  def buildKNNStructure(path:String, optData:RawParserDouble, queries:RawParserDouble, K:Int, measure:Distance, N:Int):mutable.HashMap[Int, Array[(Int, Double)]] = {
    type NumericTuple = NumericDataPoint[(Int, Double)]
    val structure = new mutable.HashMap[Int, Array[(Int, Double)]]
    val resultSets = KNN.search(optData, queries, K, measure, N)


    for(rSet <- resultSets) {
      structure += (rSet._1.get._1 -> rSet._2.sortBy(x => x._2).map(x => (x._1.get._1, x._2)))
    }

    println("Saving structure to disk...")
    val oos = new ObjectOutputStream(new FileOutputStream(path+"knnstructure"))
    oos.writeObject(structure)
    oos.close
    println("structure was saved..")

    structure
  }
}
