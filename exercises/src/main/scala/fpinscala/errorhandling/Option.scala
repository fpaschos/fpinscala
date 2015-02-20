package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

//1.
sealed trait Option[+A] {
  
  //Ok
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  //Ok
  def flatMap[B](f: A => Option[B]): Option[B] = 
    this map(f) getOrElse None

  //Fail (without pattern matching)
  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map(Some(_)) getOrElse ob

  //Ok
  def filter(f: A => Boolean): Option[A] = 
    this flatMap(a => if(f(a)) Some(a) else None)
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
  
  //2. Ok
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map( x => math.pow(x -  m,2))))
  }
  
  //Ok (comprehension variation)
//  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
//    aa <- a
//    bb <- b
//  } yield f(aa,bb)
  
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a flatMap ( aa => b map ( bb => f(aa,bb) ))
  
  //4. Fail
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
  }

  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((acc, el) => map2(acc, el)(_ :: _))

  //6. Ok
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft[Option[List[B]]](Some(Nil)) ((acc, x) => map2(f(x), acc) (_ :: _))
  }

  //Fail ( :P )
  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = 
    traverse(a)(x => x)
}