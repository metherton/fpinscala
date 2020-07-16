package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def main(args: Array[String]): Unit = {

    val numberBranches = size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))
    println("number: " + numberBranches)

    val maxBranch = maximum(Branch(Branch(Leaf(5), Leaf(3)), Branch(Leaf(7), Leaf(2))))
    println("maxBranch: " + maxBranch)

    val maxDepth = depth(Branch(Branch(Branch(Leaf(5), Leaf(6)), Leaf(3)), Branch(Leaf(7), Leaf(2))))
    println("maxDepth: " + maxDepth)

    val mapTree = map(Branch(Branch(Branch(Leaf(5), Leaf(6)), Leaf(3)), Branch(Leaf(7), Leaf(2))))(x => x * 2)
    println("mapTree : " + mapTree)

    val sizeFold = foldLeft(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"),  Branch(Leaf("d"), Leaf("x")))), 0)((acc, t) => acc + 1)
    println("sizeFold : " + sizeFold)
  }

  def foldLeft[A,B](as: Tree[A], z: B)(f: (B, Tree[A]) => B): B = // Utility functions
    as match {
      case Leaf(x) => f(z, Leaf(x))
      case Branch(l, r) => foldLeft(l, foldLeft(r, f(z, r))(f))(f)
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f),map(right)(f))
  }


  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth(left) + 1) max (depth(right) + 1)
  }

}