import scala.io.StdIn

// http://aperiodic.net/phil/scala/s-99/
object NinetyNineProblems {

  /** Problem 1: Find the last element of a list.
   *
   */
  def last(nums: List[Int]): Int = {
    if (nums.nonEmpty){
      nums.last
    }else{
      throw new IllegalArgumentException("Should provide a list with at least 1 member")
    }
  }

  /** Problem 2: Find the last but one element of a list.
   *
   */
  def penultimate(nums: List[Int]): Int = {
    if (nums.length >= 2){
      nums(nums.length-2)
    }else{
      throw new IllegalArgumentException("Should provide a list with at least 2 members")
    }
  }

  /** Problem 3: Find the kth element of a list.
   *
   */
  def nth(idx: Int, nums: List[Int]): Int ={
    if(nums.length >= idx){
      nums(idx-1)
    }else{
      throw new IllegalArgumentException("The provided list was too small")
    }
  }

  /** Problem 4: Find the number of elements of a list
   *
   */
  def length(nums: List[Int]): Int = {
    var counter = 0
    for(_ <- nums) counter+=1
    counter
  }

  /** Problem 5: Reverse a list
   *
   */
  def reverse(nums: List[Int]): List[Int] = {
    val reversed = for(x <- nums.length-1 to 0 by -1) yield nums(x)
    reversed.toList
  }

  /** Problem 6: Find out whether a list is a palindrome
   *
   */
  def isPalindrome(nums: List[Int]): Boolean = {
    nums == reverse(nums)
  }

  /** Problem 7: Flatten a nested list structure.
   *
   */
  def flatten(nums: List[Any]): List[Int] = {
    if(nums.isEmpty){
      List()
    }else{
      nums.head match {
        case x: Int => x::flatten(nums.tail)
        case x: List[Any] => flatten(x) ::: flatten(nums.tail)
      }
    }
  }

  /** Problem 8: Eliminate consecutive duplicates of list elements.
   *
   */
  def compress(list: List[Symbol]): List[Symbol] = {
    list match {
      case Nil => Nil
      case h::t::xs => if (h==t) compress(h::xs) else h::compress(t::xs)
      case c::xs => c::compress(xs)
    }
  }

  /** Problem 9: Pack consecutive duplicates of list elements into sublists.
   *
   */
  def pack(list: List[Symbol]): List[List[Symbol]] = {
    if (list == Nil || list.isEmpty){
      List(List())
    }else{
      val (currSubSeq, rest) = list.span(_ == list.head)
      if (rest == Nil) currSubSeq::Nil else currSubSeq::pack(rest)
    }
  }

  /** Problem 10: Run-length encoding of a list.
   *
   */
  def encode(list: List[Symbol]) : List[(Int, Symbol)] = {
    def encode(packedList: List[List[Symbol]]): List[(Int, Symbol)] = {
      packedList match {
        case Nil => Nil
        case x::xs => (x.length, x.head)::encode(xs)
      }
    }

    encode(pack(list))
  }

  /** Problem 11: Modified run-length encoding.
   *
   */
  def encodeModified(list: List[Symbol]): List[Any] = {
    def modifyEncoded(encodedList: List[(Int, Symbol)]): List[Any] = {
      encodedList match {
        case Nil => Nil
        case (1,x)::xs => x::modifyEncoded(xs)
        case x::xs => x::modifyEncoded(xs)
      }
    }

    modifyEncoded(encode(list))
  }

  def main(args: Array[String]): Unit = {
    //    val inputList: List[Int] =  StdIn.readLine()
    //      .split(" ")
    //      .map(_.toInt)
    //      .toList
    //
    //    var res = 0
    //    res = last(inputList)
    //    res = penultimate(inputList)
    //    res = nth(inputList.head, inputList.tail)
    //    res = length(inputList)
    //    print(res)

    //    val resList = reverse(inputList)
    //    print(resList)
    //
    //    val isInputPalindrome = isPalindrome(inputList)
    //    print(isInputPalindrome)

    //    val nestedList = List(List(1,1), 2, List(3, List(5,8)))
    //    val flattenedList = flatten(nestedList)
    //    print(flattenedList)

    val listWithDups = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    println(compress(listWithDups))
    println(pack(listWithDups))
    println(encode(listWithDups))
    println(encodeModified(listWithDups))
  }
}