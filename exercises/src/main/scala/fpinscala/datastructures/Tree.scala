package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25: Write a function `size` that counts the number of nodes (leaves and branches) in a tree
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  // Exercise 3.26: Write a function `maximum` that returns the maximum element in a `Tree[Int]`
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.27: Write a function `depth` that returns the maximum path length from the root of a tree to any leaf
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.28: Write a function `map`, analogous to the method of the same name on `List`, that modifies each element
  // in a tree with a given function
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29: Generalize `size`, `maximum`, `depth`, and `map`, writing a new function `fold` that abstracts over
  // their similarities. Reimplement them in terms of this more general function
  def fold[A, B](tree: Tree[A])(f: A => B)(b: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }

  def sizeFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximumFold(tree: Tree[Int]): Int =
    fold(tree)(v => v)(_ max _)

  def depthFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((l, r) => 1 + (l max r))

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}