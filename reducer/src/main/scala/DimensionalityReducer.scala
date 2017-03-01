import java.lang.Math.{pow, sqrt}

import breeze.stats.distributions.Gaussian
import tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 01-03-2017.
  */
object DimensionalityReducer{



  def getNewVector(x:Array[Double], matrix:Array[Array[Double]], a: => Array[Double], binary:Boolean, seed:Long) = {
    if(binary) {
      MatrixVectorProductBit(x,matrix,a, seed)//return Reduced Vector
    } else {
      MatrixVectorProduct(x,matrix,a)//return Reduced Vector
    }
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

  def MatrixVectorProductBit(x:Array[Double],matrix:Array[Array[Double]], a: => Array[Double], rndSeed:Long) = {
    val rnd = new Random(rndSeed)
    for(i <- matrix.indices) {
      a(i) = {
        val dot = Distance.parDotProduct(x, matrix(i))
        dot match {
          case dot if(dot < 0) => 0
          case dot if(dot > 0) => 1
          case dot if(dot == 0) => rnd.nextInt(2)
        }
      }
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
