import java.lang.Math.{pow, sqrt}

import breeze.stats.distributions.Gaussian
import tools.Distance

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 01-03-2017.
  */
object DimensionalityReducer{

  def getNewVector(x:Array[Double], matrix:Array[Array[Double]], a: => Array[Double], binary:Boolean, seed:Long, sqrtTargetDim:Double) = {
    if(binary) {
      MatrixVectorProductBit(x,matrix,a, seed)//return Reduced Vector
    } else {
      MatrixVectorProduct(x, matrix, a)
      scaleToTargetDimension(a, sqrtTargetDim)//return Reduced Vector
    }
  }

  def scaleToTargetDimension(a: => Array[Double], sqrtTargetDim:Double) = {
    for(i <- a.indices) {
      a(i) = a(i)/sqrtTargetDim
    }
  }

  def getRandMatrix(targetDimensions:Int, originalDimensions:Int, seed:Long): Array[Array[Double]] ={
    val randomMatrix = for {
      i <- (0 until targetDimensions).toArray
      b <- Array(new Array[Double](originalDimensions))
    } yield b

    val rnd = new Random(seed)

    // Populate bMatrix
    for (i <- 0 until targetDimensions) {
      for (j <- 0 until originalDimensions) {
        // dimenstions in each Vector
        randomMatrix(i)(j) = rnd.nextGaussian()
      }
    }
    //val M=normalizeMatrix(randomMatrix)
    randomMatrix
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
}
