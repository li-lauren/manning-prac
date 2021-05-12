import Stream._

sealed trait Stream[+A] {
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

  def toList: List[A]  = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h,t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n-1))
    case Cons(h, _) if (n == 1) => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] =
//    this match {
//      case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile p)
//      case _ => empty
//    }
   foldRight(empty)((a,b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
//    this match {
//      case Cons(h, t) => p(h()) && (t() forAll p)
//      case _ => true
//    }
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  // 5.13 implement map, take, takeWhile, zipWith and zipAll via unfold
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t),n) if (n > 1) => Some(h(), (t(), n-1))
      case (Cons(h,t), 1) => Some(h(), (empty, 1))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B,C](b: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, b)) {
      case (Cons(ha,ta), Cons(hb,tb)) => Some(f(ha(),hb()), (ta(),tb()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)) {
      case (Cons(ha,ta),Cons(hb,tb)) =>
        Some((Some(ha()),Some(hb())), (ta(), tb()))
      case (Cons(ha,ta), Empty) =>
        Some((Some(ha()),None), (ta(), Empty))
      case (Empty, Cons(hb,tb)) =>
        Some((None,Some(hb())), (Empty,tb()))
      case (Empty,Empty) => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  // why does the argument need to be non-strict here?
  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a,b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???
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
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case None => empty
    }
}

// 5.8 generalized ones
def constant[A](a: A): Stream[A] = {
//  Stream.cons(a, constant(a))
  lazy val tail: Stream[A] = Cons(() => a, () => tail)
  tail
}
// 5.9 n, n+1, n+2, ... (infinite stream)
def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

// 5.10 infinite stream of Fibonacci numbers
def fibs: Stream[Int] = {
  def go(f0: Int, f1: Int): Stream[Int] =
    cons(f0, go(f1, f0+f1))
  go(0,1)
}

// 5.11 unfold
def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }

// 5.12 redefine fibs, from, constant, and ones using unfold
def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1,1))

def constantViaUnfold[A](a: A): Stream[A] =
  unfold(a)(a => Some(a, a))

def fromViaUnfold(n: Int): Stream[Int] =
  unfold(n)(n => Some(n, n+1))

def fibsViaUnfold: Stream[Int] =
  unfold((0,1)) {
    case (f0,f1) => Some(f0,(f1, f0+f1))
  }

