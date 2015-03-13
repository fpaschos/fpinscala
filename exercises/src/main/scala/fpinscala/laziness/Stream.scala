package fpinscala.laziness
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  
  //2. Fail
  def take(n: Int): Stream[A] = this match {
    case Cons(h,t) if n > 1 => Stream.cons(h(), t() take(n-1))
    case Cons(h,_) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  //3. Fail
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  //5. Ok
  def takeWhileVieFoldRight (p: A => Boolean): Stream[A] = 
    foldRight(Stream.empty[A]) { (x, acc) => if (p(x))  Stream.cons(x, acc) else Stream.empty[A]}
  
  //4. Ok with variation
  def forAll(p: A => Boolean): Boolean = ! this.exists(!p(_))
  def forAll_1(f: A => Boolean): Boolean =  foldRight(true)((a,b) => f(a) && b)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
  
  //Exersise .1 OK Overflows for large lists (not tail recursive)
  def toList: List[A] = this.foldRight(List.empty[A])((h, acc) => h :: acc)
  
  
  //6. Ok
  def map[B](f: A => B): Stream[B] = {
    this.foldRight(Stream.empty[B]){(h, t) => Stream.cons(f(h), t)}
  }
  
  //6. Fail
  def filter(f: A => Boolean):Stream[A] = {
    this.foldRight(Stream.empty[A]) {(h,t) => if(f(h)) Stream.cons(h,t) else t}
  }
 
  //6 Fail
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => Stream.cons(h,t))
  
  //6. Fail
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B]) {(h,t) => f(h) append t}
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  //7. Ok
  def constant[A](a:A): Stream[A] = Stream.cons(a, constant(a))

  //8. Ok
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  //9. Ok
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0+f1))
    go(0, 1)
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}