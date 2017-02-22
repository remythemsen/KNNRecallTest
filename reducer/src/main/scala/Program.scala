import java.io.{BufferedWriter, File, FileOutputStream, OutputStreamWriter}
import java.util.concurrent.{ArrayBlockingQueue, Executors}
import Math._

import breeze.stats.distributions.Gaussian
import io.Parser.RawParserDouble
import tools.Distance

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

/**
  * Created by remeeh on 2/21/17.
  */

case class Config(data:String, outDir:String)
object Program extends App {
implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

  val queryPoints = Source.fromFile(new File("data/queries-10-ids.data")).getLines
  val qpMap = mutable.HashMap[Int, Boolean]()
  while(queryPoints.hasNext) {
    qpMap+=(queryPoints.next.toInt -> true)
  }
  val config = new Config("data/descriptors-40000.data", "")
  println("Generating Random Matrix")

  // TODO Find replacement of random
  val rnd = new Gaussian(0,1)

  // Random Matrix containing values from 0-1 (256 x 4096)
  val randomMatrix = DimensionalityReducer.getRandMatrix(20000000, 4096, rnd)
  // val randomMatrix: DenseMatrix[Float] = DimensionalityReducer.getRandMatrix(20000000, 4096);
  // Matrix of precomputed Vectors to build reduced vectors from (256 x 4096)



  var n = 0
  val p = 4
  // Number of threads // Same time no matter what number from 4-16
  val loadedTuples = new ArrayBlockingQueue[(Int, Array[Double])](10000)
  val preProcessedTuples = new ArrayBlockingQueue[(Int, Array[Double])](20)

  val input = new RawParserDouble(Source.fromFile(new File(config.data)).getLines)
  n = Source.fromFile(new File(config.data)).getLines.length / 2
  var progress = 0
  println(n)

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
        val aux = new Array[Double](256)
        DimensionalityReducer.getNewVector(tuple._2, randomMatrix, aux)
        val reducedTuple = (tuple._1, aux)
        preProcessedTuples.put(reducedTuple)
      }
    }.onFailure {
      case t => println("An error has occured: " + t.getMessage)
    }
  }

  val dir: String = config.outDir.concat("")
    // constructing filename
    .concat(config.data.substring(0, config.data.length - 5))
    .concat("-reduced.data")

  val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dir.toString)))

  val qpOutDir = config.outDir.concat("")
    .concat(config.data.substring(0, config.data.length - 5))
    .concat("-reduced-queries.data")

  val qpOutPut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(qpOutDir.toString)))

  var j = 0.0

  while (progress != n) {

    var t = preProcessedTuples.take()
    require(t._2.length == 256)

    var sb = new StringBuffer(456)
    sb.append(t._1)
    sb.append(" ")
    for (component <- t._2) {
      sb.append(component + " ")
    }
    sb.append("\n")

    // Write resulting set
    output.write(sb.toString())

    // if point is a querypoint, then also write to querypoint file
    if(qpMap.get(t._1).head)
      qpOutPut.write(sb.toString())


    j += 1.0
    progress += 1
    if (j % 100 == 0) {
      println("\r"+((j / n) * 100).toInt.toString + "%")
    }
  }
  output.close()
  qpOutPut.close()
  println("Finished with "+progress+" out of "+n+" tuples")
}

object DimensionalityReducer{

  def getNewVector(x:Array[Double], matrix:Array[Array[Double]], a: => Array[Double]) = {
    MatrixVectorProduct(x,matrix,a)//return Reduced Vector
  }

  def getRandMatrix(n:Int, d:Int, rnd:Gaussian): Array[Array[Double]] ={

    val epsilon=1// 0 <= epsilon <= 1
    val base2 = scala.math.log(2)
    val log2N = scala.math.log(n) / base2
    // m = new reduced dimension
    val m=((9*epsilon*log2N).toInt) + 38

    val randomMatrix = for {
      i <- (0 until m).toArray
      b <- Array(new Array[Double](d))
    } yield b

    // Populate bMatrix
    for (i <- 0 until m) {
      for (j <- 0 until d) {
        // dimenstions in each Vector
        randomMatrix(i)(j) = rnd.sample
      }
    }
    val M=normalizeMatrix(randomMatrix)
    M
  }

  def MatrixVectorProduct(x:Array[Double],matrix:Array[Array[Double]], a: => Array[Double])={
    // TODO BUild while init'ing
    for (i <- 0 until 256) {
      a(i) = Distance.parDotProduct(x, matrix(i))
    }
  }

  def normalizeMatrix(A:Array[Array[Double]]):Array[Array[Double]]={
    val buffer= new ArrayBuffer[Double]

    val B = for {
      i <- (0 until A.length).toArray
      b <- Array(new Array[Double](A(0).length))
    } yield b

    // Populate bMatrix
    for (i <- 0 until A.length) {
      for (j <- 0 until A(0).length) {
        // dimenstions in each Vector
        B(i)(j) = 0.0
      }
    }
    for(i<-0 until A.length){
      val b = new ArrayBuffer[Double]
      for(j<-0 until A(0).length){
        b+=A(i)(j) // Note this conversion
      }
      val l= sqrt(b.map { case (x) => pow(x, 2) }.sum)
      for(c <-0 until A(0).length){
        B(i)(c) = (b(c)/l)
      }
    }
    B
  }
}
