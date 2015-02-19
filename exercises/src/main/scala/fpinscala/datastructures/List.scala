package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = {
    //Ok with variation on Nil exception
    l match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    //Ok variation on Nil exception
    l match {
      case Nil => Nil
      case Cons(_,t) => Cons(h,t)
    }
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    //Ok with variation
    n match {
      case 0 => l
      case n => drop(List.tail(l), n-1)
    }
  }

  //Fail
  def dropWhile[A](l: List[A]) (f: A => Boolean): List[A] = ???
//  {
//    l match {
//      case Cons(h, t) if f(h) => dropWhile(t)(f)
//      case _ => l
//    }
//  }

  def init[A](l: List[A]): List[A] = {
    //Ok with variation Nil exception
    l match {
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
    
  }

  //Ok
  def length[A](l: List[A]): Int = foldRight(l, 0) { (_, acc:Int) => acc + 1}

  //Ok
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }
  
  //Ok variation foldRight
  def reverse[A] (xs: List[A]): List[A] = {
    foldRight(xs, Nil:List[A]) {(x, acc) => append(acc,List(x))}
  }
  
  //Ok
  def reverse2[A] (xs: List[A]): List[A] = {
    foldLeft(xs, Nil:List[A]) { (acc, x) => Cons(x, acc)}
  }

  //Ok
  def foldRightWithFL[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a,b) => f(b,a))
  }
  
  //Fail (Too hard)
  def foldLeftWithFR[A,B] (xs: List[A], z:B) (f:(B,A) =>B) :B = ???
//  {
//    foldRight(xs, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
//  }

  //Ok 
  def appendWithFL[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse2(a1), a2) {(acc, x)=> Cons(x, acc)}
  }

  def appendWithFR[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2) {Cons(_,_)}
  }
  
  //Ok
  def concat[A](l: List[List[A]]): List[A] = {
    foldLeft(l, Nil:List[A]) {append(_,_)}
  }
  
  //Ok
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int]) {(x, acc) =>Cons(x+1, acc)}
  }
  
  //17. Ok
  def toStringList(l: List[Int]): List[String] = {
    foldRight(l, Nil:List[String]) ((x, acc) => Cons(x.toString, acc))
  }
  
  //18. Ok [1 of 3 variations (foldRight, foldRightViaFoldLeft or mutation)]
  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRightWithFL(l, Nil:List[B])((h,t) => Cons(f(h),t))
  }
  
  //19. Ok with one variation as above
  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRightWithFL(l, Nil:List[A]) { (h,t)=>
      if(f(h)) Cons(h,t) else t
    }
  }
  
  //20. Ok
  def flatMap[A,B](l: List[A])(f: A => List[B]) = {
    concat(map(l)(f))
  }
  
  //21. Ok
  def filterViaFlatMap[A](l: List[A])(f: A=> Boolean) :List[A] = {
    flatMap(l) (x => if(f(x)) List(x) else Nil)
  }
  
  //22. Ok
  def addLists(xs: List[Int], ys: List[Int]): List[Int] = (xs,ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xt), Cons(y,yt)) => Cons(x+y, addLists(xt,yt))
  }
  
  //23. Ok with types (name variation zipWith)
  def mergeLists[A,B,C](xs: List[A], ys: List[B])(f: (A,B) => C): List[C] =(xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x,xt), Cons(y,yt)) => Cons(f(x,y), mergeLists(xt,yt)(f))
  }
  
  //24. Fail 
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = ???
//  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
//    case (_,Nil) => true
//    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
//    case _ => false
//  }
//  @annotation.tailrec
//  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
//    case Nil => false
//    case Cons(h,t) if startsWith(l, sub) => true
//    case Cons(h,t) => hasSubsequence(t, sub)
//  }
}