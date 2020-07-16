package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def main(args: Array[String]): Unit = {

    val maxBranch = maximum2(Branch(Branch(Leaf(5), Leaf(3)), Branch(Leaf(11), Leaf(2))))
    println("maxBranch: " + maxBranch)

    val maxBranchWithFold = maximum(Branch(Branch(Leaf(5), Leaf(3)), Branch(Leaf(11), Leaf(2))))
    println("maxBranchWithFold: " + maxBranchWithFold)

    val maxDepth = depth(Branch(Branch(Branch(Leaf(5), Leaf(6)), Leaf(3)), Branch(Leaf(7), Leaf(2))))
    println("maxDepth: " + maxDepth)


    val maxDepth2 = depth2(Branch(Branch(Branch(Leaf(5), Leaf(6)), Leaf(3)), Branch(Leaf(7), Leaf(2))))
    println("maxDepth2: " + maxDepth2)

    val mapTree = map(Branch(Branch(Branch(Leaf(5), Leaf(6)), Leaf(3)), Branch(Leaf(7), Leaf(2))))(x => x * 2)
    println("mapTree : " + mapTree)

    val sizeFold = foldLeft(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"),  Branch(Leaf("d"), Leaf("x")))), 0)((acc, t) => acc + 1)
    println("sizeFold : " + sizeFold)

    val numberBranches = size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))
    println("numberBranches: " + numberBranches)

    val numberBranches2 = size2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))))
    println("numberBranches2: " + numberBranches2)
  }

  def size[A](tree: Tree[A]): Int =
    foldLeft(tree, 0)((acc, t) => acc + 1)

  def size2[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

//  def foldLeft[A,B](as: Tree[A], z: B)(f: (B, Tree[A]) => B): B = // Utility functions
//    as match {
//      case Leaf(x) => f(z, Leaf(x))
//      case Branch(l, r) => foldLeft(l, foldLeft(r, f(z, l))(f))(f)
//    }

  def depth2[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => (depth2(left) + 1) max (depth2(right) + 1)
  }

  def depth[A](tree: Tree[A]): Int =
    foldLeft(tree, 1)((acc, t) => acc + 1)

  def foldLeft[A,B](as: Tree[A], z: B)(f: (B, A) => B): B = // Utility functions
    as match {
      case Leaf(x) => f(z, x)
      case Branch(l, r) => foldLeft(r, foldLeft(l, z)(f))(f)
    }

//  def foldLeft[A,B](as: Tree[A], z: B)(f: (B, Tree[A]) => B): B = // Utility functions
//    as match {
//      case Leaf(x) => f(z, Leaf(x))
//      case Branch(l, r) => foldLeft(l, foldLeft(r, f(z, r))(f))(f)
//    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f),map(right)(f))
  }


  def maximum(tree: Tree[Int]): Int =
    foldLeft(tree, 0)((acc, t) => acc max t)


  def maximum2(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left) max maximum(right)
  }



}