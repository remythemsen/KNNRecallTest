import java.io._

import io.Parser.RawParserDouble
import scopt.OptionParser
import tools.DataPoint.NumericDataPoint
import tools._

import scala.collection.mutable
import scala.io.Source

/**
  * Created by remeeh on 14-03-2017.
  */
object Program extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {

      val measure = config.measure.toLowerCase match {
        case "cosine" => Cosine
        case "euclidean" => Euclidean
      }

      for(i <- 0 until 10) {
        println("building structure "+(i+1)+" out of 10")
        val knnSName = "data/"+i+"/structure-"+config.measure
        val qpfile = "data/"+i+"/queries.txt"
        buildKNNStructure(new File(knnSName), new RawParserDouble(Source.fromFile(config.data).getLines()), new RawParserDouble(Source.fromFile(new File(qpfile)).getLines()), config.k, measure, config.n)
      }

    }
    case None => // Nothing
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

  def getArgsParser : OptionParser[Config] = {
    new OptionParser[Config]("KNNBuilder") {
      head("KNNBuilder", "1.0")

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

      opt[String]('o', "out").valueName("<string>").required().action((x, c) =>
        c.copy(outDir = x)).text("out file directory (without trailing slash")

      help("help").text("prints this usage text")
    }
  }
}
