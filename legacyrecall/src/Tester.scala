import java.io._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by remeeh on 12-03-2017.
  */
object Tester extends App {

  val parser = new OptionParser[Config]("Reducer") {
    head("Reducer", "1.0")

    opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
      c.copy(data = x)).text("input data file!")

    opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
      c.copy(n = x)).text("input data file size")

    opt[Int]('k', "knn").required().valueName("<int>").action((x, c) =>
      c.copy(k = x)).text("input data file size")

    opt[File]('q', "querypoints").required().valueName("<file>").action((x, c) =>
      c.copy(queryPoints = x)).text("optimal dataset query points")

    opt[File]('s', "knnstructure").required().valueName("<file>").action((x, c) =>
      c.copy(knnstructure = x)).text("optimal knn structure file")

    opt[String]('m', "measure").valueName("<string>").required().action((x, c) =>
      c.copy(measure = x)).text("measure chosen for optimal set generation")

    opt[File]('t', "testcases").valueName("<File>").required().action((x, c) =>
      c.copy(testCases = x)).text("test cases file")

    opt[String]('o', "out").valueName("<string>").required().action((x, c) =>
      c.copy(outDir = x)).text("out file directory (without trailing slash")

    help("help").text("prints this usage text")
  }

  var tknn: mutable.HashMap[Int, Array[(Int, Double)]] = _

  parser.parse(args, Config()) match {
    case Some(config) => {

      // TODO Get input params:
      // Optimal Setup
      val data = config.data //"data/descriptors-1-million.data"
      val queries = config.queryPoints //"data/queries-1m-raw.data"
      val K = config.k
      val measure = config.measure.toLowerCase match {
        case "euclidean" => Euclidean
        case "cosine" => Cosine
        case "hamming" => Hamming
      }
      val N = config.n
      val outPath = config.outDir + "/results.txt"

      println("Preparing KNN Recall Test...")
      println("Loading True KNN Structure...")
      try {
        tknn = loadKNNStructure(config.knnstructure)
      } catch {
        case e: IOException => {
          println(e)
          println("No usable optimal result structure was found!")
          // With side effect of saving the new structure to disk
          tknn = buildKNNStructure(config.knnstructure, new RawParserDouble(Source.fromFile(config.data).getLines()), new RawParserDouble(Source.fromFile(config.queryPoints).getLines()), K, measure, N)
        }
      }

      println("Loading TestCases...")
      val parser = new TestCasesParser(Source.fromFile(config.testCases).getLines())
      var testCases = new ArrayBuffer[TestCase[(Int, Array[Double])]]()
      while (parser.hasNext) {
        testCases += parser.getTestCase
      }


      println("Starting Recall Test...")
      val htmlGen = new HTMLGenerator(new StringBuilder)
      val outPutSets: ArrayBuffer[(String, String, String, String, String, Double, Double, Double)] = new ArrayBuffer()

      // Optimal resutls to be printed
      val pResults:ArrayBuffer[(Int, Array[(Int,Double)])] = new ArrayBuffer[(Int, Array[(Int,Double)])]()
      for (v <- this.tknn.iterator) {
        pResults += Tuple2(v._1,v._2)
      }

      // Adding optimal res html section
      htmlGen.addSection(Tuple2("Optimal: (Euclidean, 4096 dimensions)", pResults))



      for (testCase <- testCases) {
        // Progress test 1 out of 4
        val resultSets = testCase.run

        // Recall is the average over jaccard sim on each q result

        var avgClosestDist = 0.0
        var averageRecall = 0.0
        val optimalAvgClosestDist = {
          var sum = 0.0
          for (v <- this.tknn.valuesIterator) {
            //Assuming that they are sorted
            sum += v.head._2
          }
          sum / this.tknn.valuesIterator.length
        }

        val tcInfo = testCase.getInfo._4.toLowerCase + " " + testCase.getInfo._1 + " " + testCase.getInfo._5

        val testCaseHtmlOutSection:(String, ArrayBuffer[(Int, Array[(Int,Double)])]) = (tcInfo, new ArrayBuffer[(Int, Array[(Int,Double)])]())

        for (resultSet <- resultSets) { // Resultset represents a query to KNN, (that query's closest points)
          // Insert set into resultset to be printed in html

          val qp = resultSet._1.get._1
          val sortedResSets = resultSet._2.sortBy(x => x._2)    //.map(y => y._1).map(z => z.get)
          val closestTuples = sortedResSets.map(x => x._1.get._1).zip(sortedResSets.map(y => y._2))


          // New measure section
          val optKnn = this.tknn.get(resultSet._1.get._1).get
          val p_k = optKnn(optKnn.length-1) // Getting k'th item




          testCaseHtmlOutSection._2 += Tuple2(qp, closestTuples)

          avgClosestDist += {
            resultSet._2.sortBy(x => x._2).head._2
          }
          averageRecall += {
            val optimal: Array[(Int, Double)] = this.tknn.get(resultSet._1.get._1).get
            val optSet = optimal.map(x => x._1)
            val retrievedSet = resultSet._2.map(x => x._1.get._1)

            Jaccard.measure(optSet, retrievedSet)
          }
        }

        // Print to HTML
        htmlGen.addSection(testCaseHtmlOutSection)

        val result = averageRecall / resultSets.length
        avgClosestDist = avgClosestDist / resultSets.length
        val info = testCase.getInfo

        outPutSets += Tuple8(info._1, info._2, info._3, info._4, info._5, result, optimalAvgClosestDist, avgClosestDist)
        avgClosestDist = 0

      }
      // Write HTML
      htmlGen.outPut("data/test.html")


      Out.writeToFile(outPath, outPutSets)
      println("Test Finished.")

    }
    case None => //"nothing"
  }

  def loadKNNStructure(file:File): mutable.HashMap[Int, Array[(Int, Double)]] = {
    println("Loading Optimal KNN structure")
    val objReader = new ObjectInputStream(new FileInputStream(file))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int, Array[(Int, Double)]]]
    objReader.close()
    hashMap
  }

  def buildKNNStructure(file:File, optData: RawParserDouble, queries: RawParserDouble, K: Int, measure: Distance, N: Int): mutable.HashMap[Int, Array[(Int, Double)]] = {
    type NumericTuple = NumericDataPoint[(Int, Double)]
    val structure = new mutable.HashMap[Int, Array[(Int, Double)]]
    val resultSets = KNN.search(optData, queries, K, measure, N)


    for (rSet <- resultSets) {
      structure += (rSet._1.get._1 -> rSet._2.sortBy(x => x._2).map(x => (x._1.get._1, x._2)))
    }

    println("Saving structure to disk...")
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(structure)
    oos.close
    println("structure was saved..")

    structure
  }
}
