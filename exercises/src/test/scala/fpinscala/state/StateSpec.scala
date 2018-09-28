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

}
