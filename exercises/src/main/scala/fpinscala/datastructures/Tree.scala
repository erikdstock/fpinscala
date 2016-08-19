package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // ex 3.25 size()

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // ex 3.26 find max() element in tree

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // ex 3.27 implement depth() - max path from root to any leaf
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  // ex 3.28 implement map() to apply function to each element in tree
  // I got overconfident here and did this one without writing a test or trying to compile. when I checked my answer
  // I checked the answer I realized I wrote this without the Leaf constructor.
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    // case Leaf(v) => f(v) // my mistake
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => map(t)(f)
  }

  // ex 3.29 implement fold() to abstract similarities and reimplement size, maximum, depth and map
  /*
    I was able to implement all of these except map() pretty easily.
    Figuring out the third (middle) function param on the fold function was difficult and the 
    way I named it is a little awkward but otherwise it worked. With map in particular
    I struggled a little bit because of the type inferences- I knew
    that was the problem when I worked out a trace below, but I couldn't remember the way to 
    clarify this. In retrospect I could have looked back at my List answers to remind myself.
    mapFold() was the only exercise where I looked at the answer before getting a working
    function myself.
  */

  // early attempt - worked for sizeFold(), then i fixed it when it broke for maximumFold()
  // def fold[A, B](t: Tree[A], z: B)(f: (B, B) => B): B = t match {
  //   case Leaf(v) => z
  //   case Branch(l, r) => f(z, f(fold(l, z)(f), fold(r, z)(f)) )
  // }
  def fold[A, B](t: Tree[A])(g: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => g(v)
    case Branch(l, r) => f(fold(l)(g)(f), fold(r)(g)(f))
  }

  def sizeFold[A](t: Tree[A]): Int = 
    fold(t)(_ => 1)(1 + _ + _)

  def maximumFold(t: Tree[Int]): Int =
    fold(t)((x:Int) => x)(_ max _)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    // fold(t)( x => Leaf(f(x)) )( (Branch(_, _) ) // my original answer
    // fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_)) // answer from back of book
    fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_,_)) // my answer, fixed.


  /* trace: f= (x => x * 2)
          root 
     leaf1(1), branch2
                 leaf2(2)     leaf3(3)
    Branch( fold(leaf1)( x=>Leaf(f(x)) )( (Branch(_, _) ), fold(branch2)( x=>Leaf(f(x)) )( (Branch(_, _) )
    Branch( Leaf(4), fold(branch2)( x=>Leaf(f(x)) )( (Branch(_, _) )
    Branch( Leaf(4), Branch)
  */



  def test(): Unit = {
    /* example Tree[Int]:
                        root
             branch2              branch3
        branch4   leaf1:1    leaf2:2   branch5
    leaf3:1  leaf4:6              leaf5:5    branch6
                                        leaf6:3   leaf7:4
    */
    val leaf1 = Leaf(1)
    val leaf2 = Leaf(2)
    val leaf3 = Leaf(1)
    val leaf4 = Leaf(6)
    val leaf5 = Leaf(5)
    val leaf6 = Leaf(3)
    val leaf7 = Leaf(4)
    val branch6 = Branch(leaf6, leaf7)
    val branch5 = Branch(leaf5, branch6)
    val branch4 = Branch(leaf3, leaf4)
    val branch3 = Branch(leaf2, branch5)
    val branch2 = Branch(branch4, leaf1)
    val root = Branch(branch2, branch3)

    assert(size(root) == 13, "size: tree has 13 members (7 + 6)")
    assert(maximum(root) == 6, "max: max value in tree is 6")
    assert(depth(root) == 5, "depth: tree is 5 levels deep at max")

    println(maximumFold(root))
    assert(sizeFold(root) == 13, "size with fold: tree has 13 members (7 + 6)")
    assert(maximumFold(root) == 6, "max: max value in tree is 6")
    // assert(depthFold(root) == 5, "depth: tree is 5 levels deep at max")
  }
}