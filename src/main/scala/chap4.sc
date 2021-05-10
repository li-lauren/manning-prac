sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
  // apply f if the Option is not None
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }
}

// 4.2 variance
def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs) flatMap(m => mean(xs map(x => math.pow(x - m,2))))

// 4.3 map2 (combines 2 Option values using a binary function)
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
  a flatMap(aa => b map (bb => f(aa, bb)))

// 4.4 sequence (combines a list of Options into one Option containing a list of Some values)
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a.foldRight[Option[List[A]]](Some(Nil))((aa, aLst) => map2(aa, aLst)((_ :: _)))

def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
  a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence_1(t) map (hh :: _))
  }

// 4.5 traverse (map over list using a function that might fail, return None if it fails for any elem)
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a.foldRight[Option[List[B]]](Some(Nil))(
    (aa, bLst) => map2(f(aa), bLst)(_ :: _))

def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
  traverse(a)(x => x)
