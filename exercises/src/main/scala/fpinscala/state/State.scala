package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.1: Write a function that uses RNG.nextInt to generate a random integer between `0` and `Int.maxValue`
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng1) = rng.nextInt
    if (int < 0) (-(int + 1), rng1) else (int, rng1)
  }

  // Exercise 6.2: Write a function to generate a `Double` between `0` and `1`, not including `1`
  def double(rng: RNG): (Double, RNG) = {
    val (int, rng1) = nonNegativeInt(rng)
    (int.toDouble / (Int.MaxValue.toDouble + 1), rng1)
  }

  // Exercise 6.3: Write a function to generate an `(Int, Double)` pair
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng1) = nonNegativeInt(rng)
    val (dbl, rng2) = double(rng1)
    ((int, dbl), rng2)
  }

  // Write a function to generate a `(Double, Int)` pair
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((int, dbl), rng1) = intDouble(rng)
    ((dbl, int), rng1)
  }

  // Write a function to generate a `(Double, Double, Double)` tuple
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dbl1, rng1) = double(rng)
    val (dbl2, rng2) = double(rng1)
    val (dbl3, rng3) = double(rng2)
    ((dbl1, dbl2, dbl3), rng3)
  }

  // Exercise 6.4: Write a function to generate a list of random integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

    @annotation.tailrec
    def go(count1: Int, list: List[Int] = List.empty)(rng1: RNG): List[Int] = count1 match {
      case c if c > 0 =>
        val (int, r) = rng1.nextInt
        go(count1 - 1, int :: list)(r)
      case _ => list
    }

    (go(count)(rng), rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
