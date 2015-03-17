package fpinscala.testing

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean

  //3.Fail
//  def &&(p: Prop): Prop = new Prop {
//    def check = Prop.this.check && p.check
//  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
  

}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  //4. Fail
//  def choose(start: Int, stopExclusive: Int): Gen[Int] =
//    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
  
  /* We could write this as an explicit state action, but this is far less
  convenient, since it requires us to manually thread the `RNG` through the
  computation. */
  //4.Fail
//  def choose2(start: Int, stopExclusive: Int): Gen[Int] =
//    Gen(State(rng => RNG.nonNegativeInt(rng) match {
//      case (n,rng2) => (start + n % (stopExclusive-start), rng2)
//    }))
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

