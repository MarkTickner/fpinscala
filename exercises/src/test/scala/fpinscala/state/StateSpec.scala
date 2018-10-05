package fpinscala.state

import fpinscala.state.RNG.Simple
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  "nonNegativeInt" should "return a random positive integer`" in {
    val expected = 0

    val rng = Simple(30000000000000L)
    val (actual, _) = RNG.nonNegativeInt(rng)

    actual should be >= expected
  }

  "double" should "generate a `Double` between `0` and `1`" in {
    val rng = Simple(123)
    val (actual, _) = RNG.double(rng)

    actual should be > 0d
    actual should be < 1d
  }

}
