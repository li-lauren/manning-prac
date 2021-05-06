// 2.1 Tail-Recursive Fibonacci

def fib(n: Int): Int = {
  def go(curr_n: Int, prev1: Int, prev2: Int): Int = {
    if (curr_n == n) prev2
    else go(curr_n + 1, prev1 + prev2, prev1)
  }
  go(1, 1, 0)
}

fib(5)

// 2.2 isSorted (polymorphic functions)

def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  def loop(i: Int, ans: Boolean): Boolean = {
    if (i + 1 >= as.length) ans
    else if (ordered(as(i), as(i+1))) loop(i + 1, true)
    else false
  }
  loop(0, true)
}

isSorted(Array(1), (x: Int, y: Int) => x < y)

// 2.3 currying
// Note: => associates to the right a => b => c == a => (b => c)
def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

// 2.4 uncurrying
def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a,b) => f(a)(b)

// 2.5 compose
def compose[A,B,C](f: B => C, g: A => B): A => C =
  a => f(g(a))