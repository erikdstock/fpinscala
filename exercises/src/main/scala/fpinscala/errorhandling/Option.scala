package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case Some(a) => a
    case _ => default
  }

  // map(A => Option[B]) could return Some(None) -
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
//    if (this != None) this else ob
    map (Some(_)) getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = {
    flatMap (a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ex 4.2 failed
  def variance(xs: Seq[Double]): Option[Double] = {
    val m = flatmap (mean(xs))
    mean(map ((x: Double) => math.pow(x - m, 2)))
  }
  /*
  If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
   */

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a map (aa => aa match {
      case x :: xs => Some(x) :: sequence(xs)
      case Nil => None
    })

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sys.error("todo")
}

//sealed trait Option[+A] {
//  def map[B](f: A => B): Option[B] = this match {
//    case None => None
//    case Some(a) => Some(f(a))
//  }
//
//  def getOrElse[B>:A](default: => B): B = this match {
//    case None => default
//    case Some(a) => a
//  }
//
//  // got this one wrong. correct body is :
//  // def flatMap[B](f: A => Option[B]): Option[B] =
//  //   map(f) getOrElse None
//  // so basically this is returning the value without the option container.
//  // I think? This is weird. signature is different from map. maybe revisit TODO
//  def flatMap[B](f: A => Option[B]): Option[B] = {
//    map(f) getOrElse None // correct answer
//    //  this match {
//    //    case None => None
//    //    case Some(a) => f(a)
//  }
//
//  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
//    case None => ob
//    case Some(a) => this
//  }
//  def orElse_2[B>:A](ob: => Option[B]): Option[B] =
//  //  this map((b) => b) getOrElse ob // wrong
//    this map (Some(_)) getOrElse ob
//
//  // suggested answers are below
//  def filter(f: A => Boolean): Option[A] = this match {
//    case None => None
//    case Some(a) => if (f(a)) Some(a) else None
//  }
//  // def filter_1(f: A => Boolean): Option[A] =
//  //  flatMap(a => if (f(a)) Some(a) else None)
//  //
//  //  /* Or via explicit pattern matching. */
//  //  def filter(f: A => Boolean): Option[A] = this match {
//  //    case Some(a) if f(a) => this
//  //    case _ => None
//  //  }
//}