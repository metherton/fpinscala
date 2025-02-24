package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l, r) => max(l) max max(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

}

object TestTree extends App {
  import Tree._
  val t = Branch(Branch(Leaf(9), Leaf(4)), Leaf(23))
  println(s"size of t is ${size(t)}")
  println(s"max of t is ${max(t)}")
  println(s"depth of t is ${depth(t)}")

}