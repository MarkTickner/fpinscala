package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // Exercise 5.1: Write a function to convert a `Stream` into a `List`, which will force its evaluation and let you
  // look at it in the REPL
  def toList: List[A] = this match {
    case Cons(h, t) => List(h()) ++ t().toList
    case _ => List()
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.2: Write the function `take(n)` for returning the first `n` elements of a `Stream`
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  // Exercise 5.2: Write the function `drop(n)` for skipping the first `n` elements of a `Stream`
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // Exercise 5.3: Write the function `takeWhile` for returning all starting elements of a `Stream` that match the given
  // predicate
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  // Exercise 5.4: Implement `forAll`, which checks that all elements in the `Stream` match a given predicate. Your
  // implementation should terminate the traversal as soon as it encounters a nonmatching value
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAllFold(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5: Use `foldRight` to implement `takeWhile`
  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty)

  // Exercise 5.6: Use `foldRight` to implement `headOption`
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // Exercise 5.7: Implement `map`, `filter`, `append` and `flatMap` using `foldRight`. Part of the exercise is writing
  // your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else t)

  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8: Write a function `constant` to return an infinite `Stream` of a given value
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // Exercise 5.9: Write a function that generates an infinite stream of integers, starting from `n`, then `n + 1`,
  // `n + 2`, and so on
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // Exercise 5.10: Write a function `fibs` that generates the infinite stream of Fibonacci numbers
  def fibs: Stream[Int] = {
    def go(prev: Int, curr: Int): Stream[Int] =
      cons(prev, go(curr, prev + curr))

    go(0, 1)
  }

  // Exercise 5.11: Write a more general stream-building function called `unfold`. It takes an initial state, and a
  // function for producing both the next state and the next value in the generated stream
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // Exercise 5.12: Write fibs, from, constant, and ones in terms of unfold
  def fibsUnfold: Stream[Int] =
    unfold((0, 1)) { case (n0, n1) => Some(n0, (n1, n0 + n1)) }

  def fromUnfold(n: Int): Stream[Int] =
    unfold(n)(nn => Some(nn, nn + 1))

  def constantUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  // Exercise 5.13: Use `unfold` to implement `map`, `take`, `takeWhile`, `zipWith` and `zipAll`

  // Exercise 5.14: Implement `startsWith` using functions already written

  // Exercise 5.15: Implement `tails` using `unfold`

  // Exercise 5.16: Generalise `tails` to the function `scanRight`
}