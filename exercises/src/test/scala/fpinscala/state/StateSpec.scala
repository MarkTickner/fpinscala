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

  "intDouble" should "generate an `(Int, Double)` pair" in {
    val rng = Simple(123)
    val ((actualInt, actualDouble), _) = RNG.intDouble(rng)

    actualInt should be > 0
    actualDouble should be > 0d
  }

  "doubleInt" should "generate a `(Double, Int)` pair" in {
    val rng = Simple(123)
    val ((actualDouble, actualInt), _) = RNG.doubleInt(rng)

    actualDouble should be > 0d
    actualInt should be > 0
  }

  "double3" should "generate a `(Double, Double, Double)` tuple" in {
    val rng = Simple(123)
    val ((actualDouble1, actualDouble2, actualDouble3), _) = RNG.double3(rng)

    actualDouble1 should be > 0d
    actualDouble2 should be > 0d
    actualDouble3 should be > 0d
  }

  "ints" should "generate a list of random integers" in {
    val listSize = 5
    val rng = Simple(123)
    val (randomInts, _) = RNG.ints(count = 5)(rng)

    randomInts.size shouldEqual listSize
    randomInts.foreach(ri => randomInts.count(i => i != ri) shouldEqual (listSize - 1)) // Ensure items are unique
  }

}
