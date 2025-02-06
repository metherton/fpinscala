package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((y,x) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)((y, x) => y * x)
  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((y, x) => y + 1 )
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("cannot return tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("cannot set head of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if (f(h)) => dropWhile(t, f)
    case _ => l
  }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("cannot get init of empty list")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1 )

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }


  def map[A,B](l: List[A])(f: A => B): List[B] = ???




  def test_append(): Unit =
  {
    assert( append( Nil,             Nil ) ==           Nil, "append of two empty lists should be empty list")
    assert( append( Nil,         List(3) ) ==       List(3), "append of empty list to a list should be list")
    assert( append( List(3),         Nil ) ==       List(3), "append of list to empty list should be list")
    assert( append( List(1,2),   List(3) ) ==   List(1,2,3), "append of list to one-element list should be concatenation of lists")
    assert( append( List(1),   List(2,3) ) ==   List(1,2,3), "append of one-element list to list should be concatenation of lists")
    assert( append( List(1,2), List(3,4) ) == List(1,2,3,4), "append of two lists should be concatenation of lists")
  }
  def test_foldRight(): Unit = {
    assert(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) == List(1,2,3,4), "foldright cons failed")
  }

  def test_length(): Unit = {
    assert(length(List(1,2,3)) == 3, "length failed")
  }
  def test_foldLeft(): Unit = {
    assert(foldLeft(List(1,2,3), 0)(_ + _) == 6, "foldLeft failed")
  }
}
