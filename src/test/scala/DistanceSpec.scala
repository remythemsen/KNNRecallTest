import org.scalatest.{FlatSpec, Matchers}
import tools.{Cosine, Euclidean, Hamming}

import scala.math.BigDecimal

/**
  * Created by remeeh on 2/27/17.
  */
class DistanceSpec extends FlatSpec with Matchers {
  // Euclidean
  "Euclidean measure" should "get 0.0 on identical vectors" in {
    Euclidean.measure(Array(1.0), Array(1.0)) should be (0.0)
    Euclidean.measure(Array(1.0,1.0), Array(1.0,1.0)) should be (0.0)
    Euclidean.measure(Array(1.0,2.0), Array(1.0,2.0)) should be (0.0)
  }
  "Euclidean measure" should "get correct results" in {
    BigDecimal(Euclidean.measure(Array(1.2, 1.5, 1, 0), Array(1.9, 1.2, 1.3, 1.0))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (1.292285)
    BigDecimal(Euclidean.measure(Array(0.2, 0.5, 0, 0.2), Array(1.9, 1.0, 1.7, 1.0))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (2.582634)
    BigDecimal(Euclidean.measure(Array(1.9, 1.0, 1, 0.1), Array(1.0, 1.1, 1.6, 1.8))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (2.017424)
  }
  // Hamming
  "Hamming measure" should "get 0.0 on identical vectors" in {
    Hamming.measure(Array(1.0), Array(1.0)) should be (0.0)
    Hamming.measure(Array(1.0,1.0), Array(1.0,1.0)) should be (0.0)
    Hamming.measure(Array(1.0,2.0), Array(1.0,2.0)) should be (0.0)
  }
  "Hamming measure" should "get correct results" in {
    Hamming.measure(Array(1), Array(0)) should be (1)
    Hamming.measure(Array(1,1), Array(0,1)) should be (1)
    Hamming.measure(Array(1,1,0), Array(1,1,1)) should be (1)
    Hamming.measure(Array(1,1,0,1), Array(0,0,1,0)) should be (4)
  }
  // Cosine
  "Cosine measure" should "get 0.0 on identical vectors" in {
    BigDecimal(Cosine.measure(Array(1.0,1.0), Array(1.0,1.0))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (0.0)
    BigDecimal(Cosine.measure(Array(2.0,1.0,2.0), Array(2.0,1.0,2.0))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (0.0)
    BigDecimal(Cosine.measure(Array(1.9,1.2,2.1,50), Array(1.9,1.2,2.1,50))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (0.0)
  }
  "Cosine measure" should "get correct results" in {
    BigDecimal(Cosine.measure(Array(1.2,2.0,1.9), Array(1.2,0.1,0.9))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (0.259259)
    BigDecimal(Cosine.measure(Array(1.2,99,1.9,1.0), Array(1.2,0.1,0.9,-10))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble should be (0.996954)
  }
}
