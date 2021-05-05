// 2.1 Tail-Recursive Fibonacci

def fib(n: Int): Int = {
  def go(curr_n: Int, prev1: Int, prev2: Int): Int = {
    if (curr_n == n) prev2
    else go(curr_n + 1, prev1 + prev2, prev1)
  }
  go(1, 1, 0)
}

fib(5)