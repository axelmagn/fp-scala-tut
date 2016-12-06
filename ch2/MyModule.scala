// a comment!
/* another comment */
/** a documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if(n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](arr: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= arr.length) -1
      else if (p(arr(n))) n
      else loop(n+1)

    loop(0)
  }

  def isSorted[A](arr: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def isSortedIdx(arr: Array[A], ordered: (A,A) => Boolean, idx: Int): Boolean =
      if (idx > arr.length - 2) true
      else if (!ordered(arr(idx), arr(idx+1))) false
      else isSortedIdx(arr, ordered, idx+1)
    isSortedIdx(arr, ordered, 0)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    (a: A) => ((b: B) => f(a, b))
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -52, abs))
    println(formatResult("factorial", 7, factorial))
  }
}
