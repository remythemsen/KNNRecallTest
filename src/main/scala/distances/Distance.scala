package distances

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

    dotProduct(x, y)/(magnitude(x)*magnitude(y))
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

object Jaccard {
  def measure(x: Array[Int], y:Array[Int]) : Double = {
    ???
  }

}

