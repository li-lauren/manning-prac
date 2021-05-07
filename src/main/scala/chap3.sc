// 3.2 tail
def tail[A](as: List[A]): List[A] = as match {
  case Nil => as
  case _ :: xs => xs
}

// 3.3
def setHead[A](as:List[A], newHead: A) = as match {
  case Nil => List(newHead)
  case _ :: xs => newHead :: xs
}

// Data sharing
// 3.4
def drop[A] (l: List[A], n: Int): List[A] =
  if (n <= 0) l
  else l match {
    case Nil => Nil
    case x :: xs => drop(xs, n-1)
  }

// 3.5 dropWhile
def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case x :: xs if (f(x)) => dropWhile(xs, f)
  case _ => l
}

// 3.6 init
def init[A](l: List[A]): List[A] = l match {
  case _ :: Nil => List()
  case x :: xs => x :: init(xs)
}

init(List(1,2,3,4,5))


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

// 3.16
def addOne(l: List[Int]): List[Int] =
  foldRight(l, List[Int]())((x, acc) => x + 1 :: acc)

addOne(List(12,3,4))

// 3.17
def double2String(l: List[Double]): String =
  l match {
    case Nil => ""
    case x :: xs => x.toString + double2String(xs)
  }

// 3.18 map
def map[A,B](as: List[A])(f: A => B): List[B] =
  foldRightLeft(as, List[B]())((a, acc) => f(a) :: acc)

map(List(1,2,3))(x => x + 1)

// 3.19 filter
def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight(as, List[A]())((x, acc) => if (f(x)) x :: acc else acc)

// 3.20 flatMap
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
  foldRight(map(as)(f), List[B]())((x, acc) => x ::: acc)
}

flatMap(List(1,2,3))(i => List(i,i))

// 3.21
def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap(as)(x => if (f(x)) List(x) else List())

filterViaFlatMap(List(1,2,3))(x => x > 1)

// 3.22
def addLists(a: List[Int], b: List[Int]): List[Int] =
  (a,b) match {
    case (Nil, _) => b
    case (_, Nil) => a
    case (x::xs, y::ys) => x+y :: addLists(xs, ys)
  }

addLists(List(1,2,3), List(4,5,6))

// 3.23
def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
  (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
  }

// 3.24
//def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???


// TREES

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// 3.25 count number of nodes in tree
def size[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 1
  case Branch(l,r) => 1 + size(l) + size(r)
}

def size2[A](t: Tree[A]): Int =
  fold(t)(_ => 1)(1 + _ + _)
  //fold(t)(_ => 1)((l,r) => 1 + l + r)

// 3.26 max elem in tree
def maximum(t: Tree[Int]): Int = t match {
  case Leaf(v) => v
  case Branch(l,r) => maximum(l) max maximum(r)
}

def maximum2(t: Tree[Int]): Int =
  fold(t)(v => v)(_ max _)
  //fold(t)(v => v)((l,r) => l max r)

// 3.27 max depth of tree
def depth[A](t: Tree[A]): Int = t match {
  case Leaf(_) => 0
  case Branch(l,r) => 1 + depth(l) max depth(r)
}

def depth2[A](t: Tree[A]): Int =
  fold(t)(_ => 0)(1 + _ max _)

// 3.28 modify each elem in tree with given func
def mapTree[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
  case Leaf(v) => Leaf(f(v))
  case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
}

def mapTree2[A, B](t: Tree[A])(f: A => B): Tree[B] =
  fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))

// 3.29
def fold[A, B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
  case Leaf(v) => l(v)
  case Branch(left,right) => b(fold(left)(l)(b),fold(right)(l)(b))
}



