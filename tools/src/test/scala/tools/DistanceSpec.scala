package tools

import org.scalatest.{FlatSpec, Matchers}

object DistanceSpec extends FlatSpec with Matchers {
  "Hamming Distance measure " should "return 0 for identical vectors" in {
    val res = Hamming.measure(Array(1.0, 0.0, 1.0, 0.0, 0.0, 1.0), Array(1.0, 0.0, 1.0, 0.0, 0.0, 1.0))
    res should be (0.0)
  }
}

