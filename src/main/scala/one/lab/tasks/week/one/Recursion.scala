package one.lab.tasks.week.one
import math._

object Recursion {
  def printNTimes(n: Int, value: String): Unit =
    if (n == 1) value
    else printNTimes(n-1, value)
  def gcd(a: Long, b: Long): Long = {

    if (a == b) a
    else if (a == 1 | b == 1) 1
    else {
      if (b > a) gcd(b-a, a)
      else gcd(a-b, b)
    }
  }


  def nthFibonacciNumber(n: Int): Int =
    if (n <= 0) 0
    else if (n == 1 | n == 2) 1
    else nthFibonacciNumber(n-1) + nthFibonacciNumber(n-2)

  def tailRecursiveFibonacciNumber(n: Int): Int = {
    def tailIter(acc: Int, i: Int): Int = {
      if (i == 1 | i == 2) acc
      else tailIter(acc + tailIter(acc, i-1), i+1)
    }
    tailIter(1, 3)
  }


  print(gcd(9090, -60))
}
