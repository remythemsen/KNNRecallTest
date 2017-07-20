package recall

import java.io._

import io.Parser.{Parser, RawParserDouble, ReducedParserDouble, TestCasesParser}
import io.ResultWriter
import scopt.OptionParser
import tools.DataPoint.NumericDataPoint
import tools._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Tester extends App {

  var tknn: mutable.HashMap[Int, Array[(Int, Double)]] = _

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {


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


      val eps = Array(0.01, 0.05, 0.1, 0.2)
      val resWriter = new ResultWriter(config.outDir, "recallv2", {
        val sb = new StringBuilder
        sb.append("[ Dimensions ]\t")
        sb.append("[ Measure ]\t")
        sb.append("[ N ]\t")
        sb.append("[ K ]\t")
        sb.append("[ Binary ]\t")
        for(v <- eps) {
          sb.append("[ eps: "+v+" ]\t")
        }
        sb.toString
      })

      println("Preparing KNN Recall Test...")
      println("Loading True KNN Structure...")
      try {
        tknn = loadKNNStructure(config.knnstructure)
      } catch {
        case e: IOException => {
          println(e)
          println("No usable optimal result structure was found!")
          // With side effect of saving the new structure to disk
          tknn = if(config.dataFormat == "raw") {
            buildKNNStructure(config.knnstructure, new RawParserDouble(Source.fromFile(config.data).getLines()), new RawParserDouble(Source.fromFile(config.queryPoints).getLines()), K, measure, N)
          } else {
            buildKNNStructure(config.knnstructure, new ReducedParserDouble(Source.fromFile(config.data).getLines()), new ReducedParserDouble(Source.fromFile(config.queryPoints).getLines()), K, measure, N)
          }
        }
      }

      println("Loading TestCases...")
      // Get testcases
      val tcp = new TestCasesParser(Source.fromFile(config.testCases).getLines)

      // Run through each
      while(tcp.hasNext) {
        // result for testcase
        val recalls = new Array[Double](eps.length)
        val tc = tcp.getTestCase
        val tcResultSets = tc.run

        // Build map of non reduced vecs
        println("Building Map of Org Vectors...")
        val ogSetParser = if(config.dataFormat == "raw") {
          new RawParserDouble(Source.fromFile(config.data).getLines)
        } else {
          new ReducedParserDouble(Source.fromFile(config.data).getLines)
        }
        val tcResultSetsOrgVecsMap = buildOrgVecMap(tcResultSets, ogSetParser)
        println("Done... ")
        println("Running Recall for each query... ")

        // for each query, get recall
        for(qRes <- tcResultSets) {

          // Get p_k (k'th point dist from q)
          val qpId:Int = qRes._1.get._1
          val optKnn = this.tknn.get(qpId).get
          val p_k = optKnn(tc.K-1) // Getting k'th item (assuming asc ordering)

          val org_q_vec = tcResultSetsOrgVecsMap(qpId)

          // Calc recall for each eps
          for(i <- eps.indices) {
            // Testing real distance of each found near neighbor
            for(p <- qRes._2) {
              val org_p_vec = tcResultSetsOrgVecsMap(p._1.get._1)

              // If p'_i < p_k * (1+eps) then add 1
              if(measure.measure(org_q_vec, org_p_vec) < (p_k._2 * (1+eps(i)))) recalls(i) = recalls(i) + 1
            }
          }
        }

        // Computing mean of recalls
        for(i <- recalls.indices) {
          recalls(i) = recalls(i) / tcResultSets.length.toDouble // # of qp's
        }

        // Write result as line to file
        // [Dim] [Measure] N K Bin [Eps1] [Eps2] [Epsi]
        resWriter.writeResult({
          val sb = new StringBuilder
          sb.append(tc.dimensions.toString+"\t")
          sb.append(tc.distance.getClass.getSimpleName+"\t")
          sb.append(tc.dataSetSize+"\t")
          sb.append(tc.K.toString+"\t")
          for(recall <- recalls) {
            sb.append(recall+"\t")
          }
          sb.toString
        })

      }


    }
    case None => // Nothing
  }


  def buildOrgVecMap(queries: ArrayBuffer[(NumericDataPoint[(Int, Array[Double])], Array[(NumericDataPoint[(Int, Array[Double])], Double)])], parser:Parser[NumericDataPoint[(Int, Array[Double])]]) : mutable.HashMap[Int, Array[Double]] = {
    val map = new mutable.HashMap[Int, Array[Double]]()

    // Convert input to just list of ints
    val ids = new ArrayBuffer[Int]
    for(q <- queries) {
      // Adding qp id
      ids += q._1.get._1
      for(p <- q._2) {
        // Adding res p's id's
        ids += p._1.get._1
      }
    }
    val distinctIds:ArrayBuffer[Int] = ids.distinct

    // Traverse dataset finding needed vectors
    while(parser.hasNext) {
      val t = parser.next

      if(distinctIds.contains(t.head.get._1)) {
        map += (t.head.get._1 -> t.head.get._2)
      }
    }
    map
  }

  def loadKNNStructure(file:File): mutable.HashMap[Int, Array[(Int, Double)]] = {
    println("Loading Optimal KNN structure")
    val objReader = new ObjectInputStream(new FileInputStream(file))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int, Array[(Int, Double)]]]
    objReader.close()
    hashMap
  }

  def buildKNNStructure(file:File, optData: Parser[NumericDataPoint[(Int, Array[Double])]], queries: Parser[NumericDataPoint[(Int, Array[Double])]], K: Int, measure: Distance, N: Int): mutable.HashMap[Int, Array[(Int, Double)]] = {

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

  def getArgsParser : OptionParser[Config] = {
    new OptionParser[Config]("Reducer") {
      head("Reducer", "1.0")

      opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
        c.copy(data = x)).text("input data file!")

      opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
        c.copy(n = x)).text("input data file size")

      opt[Int]('k', "knn").required().valueName("<int>").action((x, c) =>
        c.copy(k = x)).text("K Neighbors to be found for each query")

      opt[File]('q', "querypoints").required().valueName("<file>").action((x, c) =>
        c.copy(queryPoints = x)).text("querypoints used to build optimal sets")

      opt[File]('s', "knnstructure").required().valueName("<file>").action((x, c) =>
        c.copy(knnstructure = x)).text("optimal knn structure file(if you dont have one, just enter the name of a new one which will be created")

      opt[String]('m', "measure").valueName("<string>").required().action((x, c) =>
        c.copy(measure = x)).text("measure chosen for optimal set generation")

      opt[File]('t', "testcases").valueName("<File>").required().action((x, c) =>
        c.copy(testCases = x)).text("test cases file")

      opt[String]('o', "out").valueName("<string>").required().action((x, c) =>
        c.copy(outDir = x)).text("out file directory (without trailing slash")

      opt[String]('f', "dataFormat").valueName("<string>").required().action((x, c) =>
        c.copy(dataFormat = x)).text("the format in which the input (non reduced data is in) (both queries and data) raw|reduced")

      help("help").text("prints this usage text")
    }
  }
}
