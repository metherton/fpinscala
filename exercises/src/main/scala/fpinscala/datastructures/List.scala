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

  def main(args: Array[String]): Unit = {
    val myTail = tail(Cons(1, Cons(2,Cons(3,Cons(4,Nil)))))
    println("tail: " + myTail)
    val newList = setHead(List(1,2,3,4), 5)
    println("newlist: " + newList)
    val droppedList = drop(List(1,2,3,4), 5)
    println("dropped list: " + droppedList)

    val droppedWhileList = dropWhile(List(1, 6, 1,2,3,4), (x: Int) => x < 2)
    println("dropped while list: " + droppedWhileList)

    val initList = init(List(1,2,3,4))
    println("initList: " + initList)

    val product2List = product2(List(3,4,0, 6, 2))
    println("product2List: " + product2List)

    val foldRightList = foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
    println("foldRightList: " + foldRightList)
    println(List(1,2,3))

    val lengthList = length(List(1,2,3, 4,1))
    println("lengthList: " + lengthList)

    val foldLeftList = foldLeft(List(1,2,3), 0)(_ + _)
    println("foldLeftList: " + foldLeftList)

    println("sum3 using foldLeft: " + sum3(List(1,2,3)))

    println("reverse using foldLeft: " + reverse(List(1,2,3)))

    val foldLeftViaFoldRight = foldRightViaFoldLeft(List(1,2,3), Nil: List[Int])(Cons(_,_))
    println("foldLeftViaFoldRight: " + foldLeftViaFoldRight)

    val appendLists = appendViaFoldLeft(List(1,2,3), List(4,5,6))
    println("appendLists: " + appendLists)

    val myConcat = concat(List(List(1,2,3), List(4,5,6)))
    println("myConcat: " + myConcat)

    val onlyEven = filter(List(1,2,3,4,5))(x => x % 2 == 0)
    println("onlEven: " + onlyEven)

    val fm = flatMap(List(1,2,3))(x => List(x))
    println("flatmap: " + fm)


    val rAdd2 = addTwoLists(List(1,2,3), List(4,5,6))
    println("addTwoLists: " + rAdd2)

    val zw = zipWith(List("a", "b", "c"), List("x", "y", "x"))((first: String, second: String) => first + second)
    println("zw: " + zw)

    val hasSubs = hasSubsequences(List(1,2,3,4), List(1,2))
    println("hasSubs: " + hasSubs)
  }

  def hasSubsequences[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(_, Nil) => false
    case Cons(first, Cons(second, _)) => sub match {
      case Cons(sub1First, Cons(sub2First, _)) if (sub1First == first && sub2First == second) => true
      case Cons(_, t) => hasSubsequences(t, sub)
    }
  }

  def addTwoLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoLists(t1, t2))

  }


  /*
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
  applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
  */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }


  def filterWithMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else Nil)


//  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
//    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def concat[A](listOfLists: List[List[A]]): List[A] = {
    foldLeft(listOfLists, Nil: List[A])((a: List[A], b: List[A]) => appendViaFoldLeft(a, b))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  /*
  This could also be implemented directly using `foldRight`.
  */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))


  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /*
It's common practice to annotate functions you expect to be tail-recursive with the `tailrec` annotation. If the
function is not tail-recursive, it will yield a compile error, rather than silently compiling the code and resulting
in greater stack space usage at runtime.
*/
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
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



  def appendViaFoldLeft[A, B](l: List[A], r: List[A]): List[A] = {
    foldLeft(reverse(l), r)((a: List[A], b: A) => Cons(b, a))
  }

  /*
  `append` simply replaces the `Nil` constructor of the first list with the second list, which is exactly the operation
  performed by `foldRight`.
  */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }




  /*
  The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
  when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter,
  when we discuss laziness).

  The other implementations build up a chain of functions which, when called, results in the operations being performed
  with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
  calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals
  using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
  more of theoretical interest - they aren't stack-safe and won't work for large lists.
  */
  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))


//    as match {
//      case Nil => z
//      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)


  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_ , t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("set head of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  /*
  Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
  satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
  use any of the variables introduced by the pattern.
  */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }


  /*
  Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
  solution will use a stack frame for each element of the list, which can lead to stack overflows for
  large lists (can you see why?). With lists, it's common to use a temporary, mutable buffer internal to the
  function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
  buffer is allocated internal to the function, the mutation is not observable and RT is preserved.

  Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
  doesn't require even local mutation. We'll write a reverse function later in this chapter.
  */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_,acc) => acc + 1)


  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))


  /*
  A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
  use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
  implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
  mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
  */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
}
