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

  // Exercise 3.1 - what will be the return?
  // A: 3.
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x // no, 2nd tail should start with 3
    case Nil => 42 // no, would only match Nil
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 3: x + y match the first 2 items
    case Cons(h, t) => h + sum(t) // would match + return x.sum() if previous statement hadn't matched 
    case _ => 101 // would have matched anything
  }

  // concat 2 lists
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

  // Ex 3.2: implement tail()
  // Choices for implementing on Nil: return Nil, return 0, raise or Option(None)
  // I'm chosing None because I think returning another Nil could be difficult to work with- an empty List with no end.
  // From my reading of the docs Option(None) fits the expected output better and is more safe
  // Actually I'm changing this- I think I was misreading the docs Option(None) led to a type mismatch- I thought it would be acceptable
  //  Final update- I looked at the solution regarding prefering to raise on the tail of an empty list. I was working under the assumption that I was to avoid raising based on chapter 1.
  //  However I am keeping my implementation (Nil) for further solutions since it is what I came up with and the answer key allows for it.
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // Ex 3.3: Implement setHead()
  // I updated the Nil case below after running the wiki's included tests and being told a Nil list should return itself instead of adding a head to it.
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil // Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  // Ex 3.4
  // Around here I remembered to use _ for unused variables
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l else l match {
      case Nil => Nil
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  // Ex 3.5
  // I'm interpreting this question to mean drop while the head meets f(), then stop dropping, not filter()
  // Answer gives a final default case but I what I came up with is acceptable
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  // Ex 3.6
  // Implement init(), should return a list of all elements except the last
  // Why can't this be implemented in constant time like tail()?
  // A: It needs to be called recursively to reach the final tail and drop it- O(n).
  //    tail() matches against the constructor and can return in constant time- O(1).
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // Ex 3.7
  // Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
  // def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _) // original product w/ foldRight
  // def product3(ns: List[Double]) = {
  //   def multiply(h: Double, t: List): Double = h match {
  //     case 0 => {...}
  //     case _ => h * t
  //   }
  //   foldRight(ns, 1.0)(multiply)
  // }
  // def product4(ns: List[Double]) = {
  //   if (list contains 0) 0
  //   else foldRight(ns, 1.0)(_ * _)
  // }
  // I don't see a way I could stop the stack from within the product3 definition since every foldRight has to call itself. I would probably have to scan the list for the value ahead of time (in product3) which would mean an increase from O(n) to O(2n).

  // 3.8
  //See what happens when you pass Nil and Cons themselves to foldRight, like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).[10] What do you think this says about the relationship between foldRight and the data constructors of List?
  def exThreeEight[A](l: List[A]) = foldRight(l, Nil:List[A])(Cons(_,_))
  // When passed a constructor for lists with items and Nil for empty lists, this becomes an identity function. this may be composition?

  // note- review 3.7-3.8


  // Ex 3.9
  // length using foldRight()
  // signature: foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => 1 + b)
  }
  /* 
  trace for List(1, 2, 3)
    1 + foldRight(List(2, 3))((a, b) => 1 + b)
    1 + 1 + foldRight(List(3)) ...
    1 + 1 + 1 + foldRight(List()) ...
    1 + 1 + 1 + 0
  */


  // ex 3.10 foldLeft with tailrec
  // use z as acc
  // my solution is identical to the answer minus the use of the extra val- answer key says
  // foldLeft(t, f(z,h))(f) - I think this is just a mindset I need to get into.
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => {
      // z is acc so put it first
      val acc = f(z, h)
      foldLeft(t, acc)(f)
    }
  }
  // ex 3.11 sum, product and length with foldLeft
  // I first tried to do this with a type List[A] but got an overloaded method compilation error.
  // Looking back at the original foldRight method I see what it was doing- I confused the utility of foldRight's flexibility with types with sum itself.
  // After this exercise I'm not actually clear on the difference ITO why I'd ever prefer foldRight over left.
  def sumLeft(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]) = foldLeft(l, 1.0)((acc, h) => acc * h)
  def lengthLeft[A](l: List[A]) = foldLeft(l, 0)((acc, _) => acc + 1)

  // ex 3.12 reverse()
  // List(1,2,3) is the tail(2,3) reversed + the head
  // with accumulator just add head to starting list (now the tail)
  def reverse[A](l: List[A]) = foldLeft(l, Nil:List[A])((acc, h) => Cons(h, acc))

  // ex 3.13 [hard] implement foldLeft ITO of foldRight and vice versa
  // TODO: revisit this. looks doable.

  // ex 3.14 implement append() with a fold.
  // 
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

  def test_sum(sum: List[Int] => Int): Unit = {
    assert( sum(           Nil ) ==  0, "sum of empty list should be 0")
    assert( sum(       List(5) ) ==  5, "sum of single-element list should be the element" )
    assert( sum( List(1,2,3,4) ) == 10, "sum of list should be sum of its elements" )
  }

  def test_sum(): Unit = test_sum(sum)
  def test_sum2(): Unit = test_sum(sum2)

  def test_product(product: List[Double] => Double): Unit = {
    assert( product( Nil)                       ==  1.0,  "product of empty list should be 1.0" )
    assert( product( List(7.0))                 ==  7.0,  "product of single-element list should be the element" )
    assert( product( List(1.0, 2.0, 3.0, 4.0) ) == 24.0,  "product of list should be product of its elements" )
    assert( product( List(1.0, 2.0, 0.0, 4.0) ) ==  0.0,  "product of list containing zero should be zero" )
  }

  def test_product(): Unit = test_product(product)
  def test_product2(): Unit = test_product(product2)

  def test_append(append: (List[Int], List[Int]) => List[Int] ): Unit = {
    assert( append( Nil,             Nil ) ==           Nil, "append of two empty lists should be empty list")
    assert( append( Nil,         List(3) ) ==       List(3), "append of empty list to a list should be list")
    assert( append( List(3),         Nil ) ==       List(3), "append of list to empty list should be list")
    assert( append( List(1,2),   List(3) ) ==   List(1,2,3), "append of list to one-element list should be concatenation of lists")
    assert( append( List(1),   List(2,3) ) ==   List(1,2,3), "append of one-element list to list should be concatenation of lists")
    assert( append( List(1,2), List(3,4) ) == List(1,2,3,4), "append of two lists should be concatenation of lists")
  }

  def test_append_1(): Unit = test_append(append)
  def test_append_fold: Unit = test_append(appendFold)

  def test_tail(): Unit = {
    assert( tail(         Nil ) ==       Nil, "tail of Nil should be Nil")
    assert( tail(     List(3) ) ==       Nil, "tail of single-element list should be Nil")
    assert( tail( List(1,2,3) ) == List(2,3), "tail of list should be rest")
  }

  def test_setHead(): Unit = {
    assert( setHead(       Nil, 1 ) ==       Nil, "setHead of empty list should be empty list")
    assert( setHead(   List(2), 1 ) ==   List(1), "setHead of single-element list should be two-element list")
    assert( setHead( List(3,2), 1 ) == List(1,2), "setHead of two-element list should be three-element list")
  }

  def test_drop(): Unit = {
    assert( drop( Nil,          0) ==         Nil, "drop of zero elements from empty list is empty list")
    assert( drop( Nil,          1) ==         Nil, "drop of one element from empty list is empty list")
    assert( drop( Nil,         10) ==         Nil, "drop of many elements from empty list is empty list")
    assert( drop( List(3),      0) ==     List(3), "drop of zero elements from single-element list is the list")
    assert( drop( List(3),      1) ==         Nil, "drop of one element from single-element list is empty list")
    assert( drop( List(3),     10) ==         Nil, "drop of many elements from single-element list is empty list")
    assert( drop( List(1,2,3),  0) == List(1,2,3), "drop of zero elements from list is list")
    assert( drop( List(1,2,3),  1) ==   List(2,3), "drop of one elements from list is list without 1st element")
    assert( drop( List(1,2,3),  2) ==     List(3), "drop of n elements from list is list without 1st n elements")
    assert( drop( List(1,2,3), 10) ==         Nil, "drop of too many elements from list is empty list")
  }

  def test_dropWhile(): Unit = {
    val positive = (x: Int) => x > 0
    assert( dropWhile(                  Nil, positive ) ==                  Nil, "dropWhile of empty list should be empty list")
    assert( dropWhile(              List(1), positive ) ==                  Nil, "dropWhile of list with single valid element should be empty list")
    assert( dropWhile( List( 1,  2,  3,  4), positive ) ==                  Nil, "dropWhile of list with only valid elements should be empty list")
    assert( dropWhile( List( 1,  2, -3,  4), positive ) ==          List(-3, 4), "dropWhile of list with two leading valid elements should be list without leading elements")
    assert( dropWhile( List( 1, -2, -3,  4), positive ) ==      List(-2, -3, 4), "dropWhile of list with one leading valid element should be list without leading element")
    assert( dropWhile( List(-1, -2, -3,  4), positive ) ==  List(-1, -2, -3, 4), "dropWhile of list with no leading valid elements should be same list")
    assert( dropWhile( List(-1, -2, -3, -4), positive ) == List(-1, -2, -3, -4), "dropWhile of list with no valid elements should be Nil")
  }

  def test_init(): Unit = {
    assert( init(         Nil ) ==       Nil, "init of empty list should be empty list")
    assert( init(     List(3) ) ==       Nil, "init of single-element-list should be empty list")
    assert( init( List(1,2,3) ) == List(1,2), "init of list should not have last element")
  }

  def test_length(): Unit = {
    assert( length(         Nil ) == 0, "length of empty list is zero")
    assert( length(     List(1) ) == 1, "length of single-element list is one")
    assert( length( List(1,2,3) ) == 3, "length of n-element list is n")
  }

  def test_foldLeft(): Unit = {
    assert( foldLeft(  List(1, 2, 3, 4, 5), 0) (_ + _) ==
      foldRight( List(1, 2, 3, 4, 5), 0) (_ + _),
      "foldLeft should compute the same sum value as foldRight")

    assert( foldLeft(  List(1, 2, 3, 4, 5), 1) (_ * _) ==
      foldRight( List(1, 2, 3, 4, 5), 1) (_ * _),
      "foldLeft should compute the same product value as foldRight")

    assert( foldLeft(  List("a", "b", "c"), "") (_ + _) ==
      foldRight( List("a", "b", "c"), "") (_ + _),
      "foldLeft should compute the same concatenation value as foldRight")
  }

  def test_reverse(): Unit = {
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1), "reversed list should be reversed.")
    assert(reverse(Nil) == Nil, "Empty list reversed is empty")
    assert(reverse(List(5)) == List(5), "1-length list is same")
  }

  def test(): Unit = {
    test_sum
    test_sum2
    test_product
    test_product2
    test_append_1
    test_append_fold
    test_tail
    test_setHead
    test_drop
    test_dropWhile
    test_init
    test_length
    test_foldLeft
    test_reverse
  }
}
