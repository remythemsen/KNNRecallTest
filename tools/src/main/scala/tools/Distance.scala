package tools

import java.util.concurrent.Executors

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

trait Distance {
  def measure(x:Array[Double], y:Array[Double]) : Double
  def measure(x:Array[Float], y:Array[Float]) : Float
}

// Sequential Measures

object Euclidean extends Distance {
  override def measure(x: Array[Double], y: Array[Double]): Double = {
    require(x.length.equals(y.length))
    Math.sqrt((x zip y).map {
      case (a, b) => Math.pow(b - a, 2)
    }.sum)
  }

  override def measure(x: Array[Float], y: Array[Float]): Float = ???
}

object Cosine extends Distance {
  override def measure(x: Array[Double], y: Array[Double]) : Double = {

    def magnitude(x: Array[Double]) : Double = {
      var r:Double = 0.0
      for(c <- x) {
        r+=Math.pow(c, 2)
      }
      Math.sqrt(r)
    }
    def dotProduct(x: Array[Double], y: Array[Double]) : Double = {
      require(x.length.equals(y.length))
      var sum:Double = 0.0
      for(i <- x.indices) {
        sum+=x(i)*y(i)
      }
      sum
    }

    //1- to convert to similarty measure
    1-dotProduct(x, y)/(magnitude(x)*magnitude(y))
  }

  override def measure(x: Array[Float], y: Array[Float]): Float = {

    def magnitude(x: Array[Float]) : Float = {
      var r:Float = 0.0f
      for(c <- x) {
        r+=Math.pow(c, 2f).toFloat
      }
      Math.sqrt(r).toFloat
    }
    def dotProduct(x: Array[Float], y: Array[Float]) : Float = {
      require(x.length.equals(y.length))
      var sum:Float = 0.0f
      for(i <- x.indices) {
        sum+=x(i)*y(i)
      }
      sum
    }

    dotProduct(x, y)/(magnitude(x)*magnitude(y))
  }
}

object Hamming extends Distance {
  // TODO, instead of int arrays use bits
  def measure(x: Array[Int], y:Array[Int]) : Double = {
    require(x.length == y.length)
    var c:Int = 0
    for(i <- x.indices) {
      if(x(i) != y(i)) c+=1
    }
    c.toDouble
  }
  // TODO, solve this generic problem with primitives
  override def measure(x: Array[Double], y: Array[Double]): Double = {
    require(x.length == y.length)
    var c = 0.0
    for(i <- x.indices) {
      if(x(i) != y(i)) c+=1
    }
    c.toDouble
  }

  override def measure(x: Array[Float], y: Array[Float]): Float = ???
}

object Jaccard extends Distance {
  override def measure(x: Array[Double], y: Array[Double]): Double = ???

  override def measure(x: Array[Float], y: Array[Float]): Float = ???

  def measure(xs: Array[Int], ys: Array[Int]):Double = {
    xs.intersect(ys).distinct.size / xs.union(ys).distinct.size.toDouble
  }
}

object Distance {
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))
  def parDotProduct(x: Array[Double], y: Array[Double]): Double = {
    val p = 2
    val futures:ArrayBuffer[Future[Double]] = new ArrayBuffer()
    for(i <- 0 until p) {
      futures += Future {
        var r:Double = 0.0f
        for(j <- i until x.length by p) {
          r += x(j) * y(j)
        }
        r
      }
    }
    // Merge
    val results = Await.result(Future.sequence(futures), 5.seconds)
    results.sum

  }
}
