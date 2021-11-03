import Chapter3.List._

object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //    ex 3.2
    def tail[A](ds: List[A]): List[A] = ds match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    //    ex 3.3
    def setHead[A](ds: List[A], newHead: A) = {
      Cons(newHead, tail(ds))
    }

    //    ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case _ => drop(tail(l), n - 1)
    }

    //    ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, _) => if (f(x)) {
        dropWhile(tail(l), f)
      } else {
        l
      }
    }

    // ex 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    // ex 3.1
    //result 3
    println(x)

    println(tail(List("a", "b", "c")))
    println(tail(List()))
    println(drop(List(1,2,3,4,5), 3))
    val f: Int=> Boolean = a => a < 4
    println(dropWhile(List(1,2,3,4,5,6), f))
  }
}