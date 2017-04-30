import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}

import io.Parser.RawParserDouble
import scopt.OptionParser

import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 14-03-2017.
  */
object Program extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {

      val trandom = new Random(config.seed)

        val input = Source.fromFile(config.data).getLines
        val dataFileName = config.data.getName
        val qpOutPut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(config.outFile+"/"+dataFileName.substring(0, dataFileName.length-5) + ".queries")))
        val seed:Long = config.seed // use once, generated from random.org
        val rnd = new Random(trandom.nextLong())
        val skipSize = config.skipSize
        var counter: Int = 1
        var qs = 0

        while(counter <= config.n && input.hasNext) {
          val id = input.next()
          val vec = input.next()

          var randI = rnd.nextInt(skipSize-1) + 1
          val skipStep = randI % skipSize

          if (counter % skipStep == 0) {
            val randG = rnd.nextGaussian()
            if (randG < -1 || randG > 1) {
              val sb = new StringBuilder
              sb.append(id+"\n")
              sb.append(vec+"\n")
              qpOutPut.write(sb.toString)
              qs = qs + 1
            }
          }
          counter = counter + 1
        }

        qpOutPut.flush()
        qpOutPut.close()
        println(qs)
      }

    case None => // Nothing
  }

  def getArgsParser: OptionParser[Config] = {
    new OptionParser[Config]("KNNBuilder") {
      head("KNNBuilder", "1.0")

      opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
        c.copy(data = x)).text("input data file ONLY TAKES RAW!!")

      opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
        c.copy(n = x)).text("input data file size")

      opt[Int]('i', "skipSize").required().valueName("<int>").action((x, c) =>
        c.copy(skipSize = x)).text("determines the size of the queries")

      opt[Int]('s', "seed").required().valueName("<int>").action((x, c) =>
        c.copy(seed = x)).text("random seed")

      opt[String]('o', "outFile").valueName("<string>").required().action((x, c) =>
        c.copy(outFile = x)).text("out file")

      help("help").text("prints this usage text")
    }
  }
}
