package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  // this.map(f).getOrElse(None)
  // (Some(Option[B])).getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  // map(Some(_)) getOrElse ob
  // Some(x) getOrElse ob         ||   None getOrElse ob


  def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None )
  //  Some(2).filter(x=> x % 2 != 0) >> f
  // Some(2) flatMap(x => if (f(x)) Some(x) else None)
  // Some(2) map(x => if (f(x)) Some(x) else None) getOrElse None
  // x = 2    Some(if (false) Some(x) else None)
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

  // ex 4.2 the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))
//    xs map (x => math.pow(x - m, 2))
  }

  /* 4.3 Write a generic function map2 that combines two Option values using a binary function. If either Option value is None, then the return value is too. Here is its signature: */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = b flatMap((i: B) => a map ((h: A) => f(h, i)))
// a flatMap (aa => b map (bb => f(aa, bb))) Book answer


  /* 4.4
  Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original list.
  If the original list contains None even once, the result of the function should be None;
  else should be Some with a list of all the values.
  */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
//    case x :: xs => x.flatMap(aa => aa :: sequence(xs))
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _)) // so hard.
  }

  // was getting errors that basically my Some(Nil) type was too general. had to add the option[list[b]] to the foldRight call
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((x, z) => f(x).flatMap(xx => z.map(xx :: _)))
//   a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))  // the book answer

}
