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

  val dimensions = args(2).toInt
  val queryPoints = Source.fromFile(new File(args(1))).getLines
  val qpMap = mutable.HashMap[Int, Boolean]()
  while(queryPoints.hasNext) {
    qpMap+=(queryPoints.next.toInt -> true)
  }
  val config = new Config(args(0), "")

  // TODO Find replacement of random
  val rnd = new Gaussian(0,1)

  println("Generating Random Matrix")

  val p = 4
  // Number of threads // Same time no matter what number from 4-16
  val loadedTuples = new ArrayBlockingQueue[(Int, Array[Double])](10000)
  val preProcessedTuples = new ArrayBlockingQueue[(Int, Array[Double])](20)

  val input = new RawParserDouble(Source.fromFile(new File(config.data)).getLines)
  val originalDimensions = new RawParserDouble(Source.fromFile(new File(config.data)).getLines).getTuple.get._2.length
  val n = Source.fromFile(new File(config.data)).getLines.length / 2
  val randomMatrix = DimensionalityReducer.getRandMatrix(dimensions, originalDimensions , rnd)
  var progress = 0

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
    .concat("-reduced-"+dimensions+".data")

  val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dir.toString)))

  val qpOutDir = config.outDir.concat("")
    .concat(config.data.substring(0, config.data.length - 5))
    .concat("-reduced-"+dimensions+"-queries.data")

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

object DimensionalityReducer{

  def getNewVector(x:Array[Double], matrix:Array[Array[Double]], a: => Array[Double]) = {
    MatrixVectorProduct(x,matrix,a)//return Reduced Vector
  }

  def getRandMatrix(targetDimensions:Int, originalDimensions:Int, rnd:Gaussian): Array[Array[Double]] ={
    val randomMatrix = for {
      i <- (0 until targetDimensions).toArray
      b <- Array(new Array[Double](originalDimensions))
    } yield b

    // Populate bMatrix
    for (i <- 0 until targetDimensions) {
      for (j <- 0 until originalDimensions) {
        // dimenstions in each Vector
        randomMatrix(i)(j) = rnd.sample
      }
    }
    // TODO Why do we need normalizing ?
    val M=normalizeMatrix(randomMatrix)
    M
  }

  def MatrixVectorProduct(x:Array[Double],matrix:Array[Array[Double]], a: => Array[Double])={
    // TODO BUild while init'ing
    for (i <- 0 until matrix.length) {
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
