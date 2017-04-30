import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}

import io.Parser.{RawParserDouble, ReducedParserDouble}
import io.Parser.RawParserDouble
import scopt.OptionParser

import scala.io.Source
import scala.util.Random
import scala.io.Source

/**
  * Created by remeeh on 25-04-2017.
  */

object Program extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {
      val data = config.data
      val qps = config.queries
      val parser = Source.fromFile(data).getLines
      val qpparser = new RawParserDouble(Source.fromFile(qps).getLines())
      var qp = qpparser.next.get.get
      val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(config.outFile +"/"+ data.getName().substring(0, data.getName.length - 5) + ".queries")))

      while (parser.hasNext) {
        val line = parser.next.split(" ")
        val sb = new StringBuilder
        if (line.head.toInt == qp._1) {
          // Write to file
          sb.append(line.head)
          var i = 0
          val rest = line.tail
          while (i < rest.length) {
            sb.append(" " + rest(i))
            i += 1
          }
          sb.append("\n")
          output.write(sb.toString)
          if (qpparser.hasNext)
            qp = qpparser.next.get.get
        }
      }
      output.flush()
      output.close()
    }
    case None => {
      // DO nothing
    }
  }

  def getArgsParser: OptionParser[Config] = {
    new OptionParser[Config]("KNNBuilder") {
      head("KNNBuilder", "1.0")

      opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
        c.copy(data = x)).text("input data file ONLY TAKES REDUCED!!")

      opt[File]('q', "queries").required().valueName("<file>").action((x, c) =>
        c.copy(queries = x)).text("file containting queries in raw format")

      opt[String]('o', "outPath").valueName("<string>").required().action((x, c) =>
        c.copy(outFile = x)).text("out path")

      help("help").text("prints this usage text")
    }
  }
}
