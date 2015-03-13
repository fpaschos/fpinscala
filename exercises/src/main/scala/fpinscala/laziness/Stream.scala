package fpinscala.laziness


trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
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
    case Cons(h, t) if n > 1 => Stream.cons(h(), t() take (n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  //?. Ok Book version mismatch
  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  //3. Fail
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => Stream.cons(h(), t() takeWhile p)
    case _ => Stream.empty
  }

  //5. Ok
  def takeWhileVieFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (x, acc) => if (p(x)) Stream.cons(x, acc) else Stream.empty[A]}

  //4. Ok with variation
  def forAll(p: A => Boolean): Boolean = !this.exists(!p(_))

  def forAll_1(f: A => Boolean): Boolean = foldRight(true)((a, b) => f(a) && b)



  //Exersise .1 OK Overflows for large lists (not tail recursive)
  def toList: List[A] = this.foldRight(List.empty[A])((h, acc) => h :: acc)


  //6. Ok
  def map[B](f: A => B): Stream[B] = {
    this.foldRight(Stream.empty[B]) { (h, t) => Stream.cons(f(h), t)}
  }

  //6. Fail
  def filter(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A]) { (h, t) => if (f(h)) Stream.cons(h, t) else t}
  }

  //6 Fail
  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => Stream.cons(h, t))

  //6. Fail
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B]) { (h, t) => f(h) append t}

  //12. Fail
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  //12. Ok
  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), n) if n == 1 => Some((h(), (Stream.empty, n - 1)))
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }


  //12. Ok
  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  //12. Fail
   def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2,t2)) => Some(f(h1(), h2()), (t1(),t2()))
      case _ => None
    }

  //12.Ok
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWith(s2)((_,_))





  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  //13. Fail Book version mismatch but the original idea was correct!
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }


  //14. Fail book version mismatch (? empty does not appear it signals the empty stream!!!)
  def tails: Stream[Stream[A]] = Stream.unfold(this) {
    case Empty => None
    case s => Some((s, s drop 1))
  } append (Stream.empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  //15. Fail
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2

  def filterScan(f: A => Boolean): Stream[A] =
    scanRight(Stream.empty[A])((h,t) =>
      if (f(h)) Stream.cons(h, t)
      else t).flatMap(x => x)

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
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  //8. Ok
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //9. Ok
  val fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  //10. Fail
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, t)) => cons(h, unfold(t)(f))
    case None => empty
  }

  //11. Ok
  def constant_1[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  //11. Ok
  def from_1(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  //11. Ok
  def ones_1: Stream[Int] = constant_1(1)

  //11. Ok
  def fibs_1: Stream[Int] = unfold((0, 1)) { prev =>
    val next = prev._1 + prev._2
    Some(prev._1, (prev._2, next))
  }
}