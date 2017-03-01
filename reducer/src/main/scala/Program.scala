import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.util.concurrent.{ArrayBlockingQueue, Executors}
import breeze.stats.distributions.Gaussian
import io.Parser.RawParserDouble
import scopt.OptionParser
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 2/21/17.
  */

// Args config
case class Config(
                   data:File = new File("."),
                   n:Int = 0,
                   queryPoints:File = new File("."),
                   outDir:String = ".",
                   dataDim:Int = 4096,
                   targetDim:Int = 256,
                   binary:Boolean = false,
                   randomSeed:Long = System.currentTimeMillis()
                 )

object Program extends App {
  val parser = new OptionParser[Config]("Reducer") {
    head("Reducer", "1.0")

    opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
      c.copy(data = x)).text("input data file!")

    opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
      c.copy(n = x)).text("input data file size")

    opt[File]('q', "querypoints").required().valueName("<file>").action((x, c) =>
      c.copy(queryPoints = x)).text("input query point id's to be generated in separate file!")

    opt[String]('o', "out").valueName("<string>").required().action((x, c) =>
      c.copy(outDir = x)).text("out file directory (without trailing slash")

    opt[Int]('d', "dataDimension").valueName("<int>").required().action((x, c) =>
      c.copy(dataDim = x)).text("input vector dimensions, default 4096")

    opt[Int]('t', "targetDimension").valueName("<int>").required().action((x, c) =>
      c.copy(targetDim = x)).text("target vector dimensions, default 256")

    opt[Boolean]('b', "binary").valueName("<false|true>").required().action((x, c) =>
      c.copy(binary = x)).text("should out file contain binary values, default false")

    opt[Long]('r', "seed").valueName("<long>").action((x, c) =>
      c.copy(randomSeed = x)).text("random seed for matrix generation, default system time")

    help("help").text("prints this usage text")
  }

  parser.parse(args, Config()) match {
    case Some(config) => {

      implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

      val dimensions = config.targetDim
      val queryPoints = Source.fromFile(config.queryPoints).getLines
      val qpMap = mutable.HashMap[Int, Boolean]()
      while(queryPoints.hasNext) {
        qpMap+=(queryPoints.next.toInt -> true)
      }

      // TODO Find replacement of random
      val rnd = new Gaussian(0,1)


      val p = 4
      // Number of threads // Same time no matter what number from 4-16
      val loadedTuples = new ArrayBlockingQueue[(Int, Array[Double])](10000)
      val preProcessedTuples = new ArrayBlockingQueue[(Int, Array[Double])](20)

      println("Loading files...")
      val input = new RawParserDouble(Source.fromFile(config.data).getLines)
      val originalDimensions = config.dataDim
      val n = config.n

      println("Generating Random Matrix...")

      val randomMatrix = DimensionalityReducer.getRandMatrix(dimensions, originalDimensions , rnd)
      val random = new Random(config.randomSeed) // TODO Better random

      val binary = config.binary
      var progress = 0

      println("Starting reduction...")

      Future {
        while (input.hasNext) {
          loadedTuples.put(input.next.head.get)

        }
      }.onFailure {
        case t => println("An error has occured in the input parser: " + t.getMessage)
      }

      for (i <- 0 until p) {
        Future {
          while (true) {
            var tuple = loadedTuples.take()
            require(tuple._2.length == originalDimensions)
            val aux = new Array[Double](dimensions)
            DimensionalityReducer.getNewVector(tuple._2, randomMatrix, aux, binary, random.nextLong)
            val reducedTuple = (tuple._1, aux)
            preProcessedTuples.put(reducedTuple)
          }
        }.onFailure {
          case t => println("An error has occured: " + t.getMessage)
        }
      }

      val isBinary = {
        if(binary)
          "-"+config.binary
        else ""
      }
      val dir: String = config.outDir.concat("/")
        // constructing filename
        .concat(config.data.getName.substring(0, config.data.getName.length - 5))
        .concat("-reduced-"+dimensions+isBinary+".data")

      val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dir.toString)))


      val qpOutDir = config.outDir.concat("/")
        .concat(config.data.getName.substring(0, config.data.getName.length - 5))
        .concat("-reduced-"+dimensions+isBinary+".queries")

      val qpOutPut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(qpOutDir.toString)))

      var j = 0.0
      val percentile = n / 100

      while (progress != n) {

        var t = preProcessedTuples.take()
        require(t._2.length == dimensions)

        var sb = new StringBuffer(456)
        sb.append(t._1)
        sb.append(" ")
        for (component <- t._2) {
          if(binary)
            sb.append(component.toInt + " ")
          else
            sb.append(component + " ")
        }
        sb.append("\n")

        // Write resulting set
        output.write(sb.toString())

        // if point is a querypoint, then also write to querypoint file
        if(qpMap.contains(t._1))
          qpOutPut.write(sb.toString())


        j += 1.0
        progress += 1
        if (j % percentile == 0) {
          println(((j / n) * 100).toInt + "%")
        }
      }
      output.close()
      qpOutPut.close()
      println("\nFinished with "+progress+" out of "+n+" tuples...")
      print("Checking dataset...")
      val newFile = Source.fromFile(new File(dir)).getLines()
      require(newFile.length == n)
      print("Ok!\n")

    }
    case None => "Error"
  }
}

