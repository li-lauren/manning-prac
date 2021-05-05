// 3.9
def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case x :: xs => f(x, foldRight(xs, z)(f))
}
def length[A](as: List[A]): Int = foldRight(as, 0)((x,y) => 1 + y)
length(List(1,2,3))

// 3.10
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case x :: xs => foldLeft(xs, f(z, x))(f)
}

// 3.11
def sum(as: List[Int]): Int = foldLeft(as, 0)((x, y) => x + y)
def product(as: List[Int]): Int = foldLeft(as, 1)((x, y) => x * y)
def length[A](as: List[A]): Int = foldLeft(as, 0)((x,y) => x + 1)

// 3.12
def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((rest: List[A], x: A) => x :: rest)

def reverseR[A](as: List[A]): List[A] = foldRight(as, List[A]())((x: A, rest: List[A]) => rest ::: List(x))
reverseR(List("b", "l", "a", "h"))

// 3.13
// foldLeft via foldRight
def foldLeftRight[A,B](as: List[A], z:B)(f: (B, A)=> B): B =
  foldRight(reverseR(as), z)((a, b) => f(b, a))

// foldRight via foldLeft
def foldRightLeft[A,B](as: List[A], z:B)(f: (A, B)=> B): B =
  foldLeft(reverse(as), z)((b, a) => f(a, b))
// comparing values for a non-commutative f
foldLeft(List(1,2,3,4), 0)((x,y)=> 2*x - y)
foldRight(List(1,2,3,4), 0)((x,y)=> 2*x - y)
foldLeftRight(List(1,2,3,4), 0)((x,y)=> 2*x - y)
foldRightLeft(List(1,2,3,4), 0)((x,y)=> 2*x - y)

// 3.15
def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
  case Nil => ys
  case x :: xs => x :: concat(xs, ys)
}

def Concat[A](xs: List[A], ys: List[A]): List[A] =
  //foldRight(xs, ys)((x, rest) => x :: rest)
  (xs foldRight ys)(_ :: _)

Concat(List(1,2,3,4,5), List(10,12,14,16))
