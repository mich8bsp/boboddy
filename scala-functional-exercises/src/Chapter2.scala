object Chapter2 {

  def abs(n: Int): Int = {
    if (n<0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The abs value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(isSorted(Array(1.9, 2.9, 1.4, 3.5), (a: Double,b: Double)=> a<=b))
  }

  //  ex 2.2
  def isSorted[A](as: Array[A], ordered: (A,A)=>Boolean): Boolean ={
    @annotation.tailrec
    def nextIsOrdered(currentIndex: Int, nextIndex: Int): Boolean={
      if (nextIndex >= as.length) true
      else if (!ordered(as(currentIndex), as(nextIndex))) false
      else nextIsOrdered(nextIndex, nextIndex+1)
    }
    nextIsOrdered(0, 1)
  }

  //  ex 2.3
  def curry[A,B,C](f: (A, B)=>C): A => (B => C) = {
    a => b => f(a, b)
  }

  //  ex 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a,b) => f(a)(b)
  }

  //  ex 2.5
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = {
    a=>f(g(a))
  }
}