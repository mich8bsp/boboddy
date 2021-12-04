object Problem38 {

  object Solution {

    def countConsecutiveCharacters(chars: List[Char]): List[(Char, Int)] = chars match {
      case Nil => Nil
      case x::xs =>
        val (sameChar, rest) = chars.span(_ == x)
        (x, sameChar.size) :: countConsecutiveCharacters(rest)
    }

    def countAndSay(n: Int): String = n match {
      case 1 => "1"
      case _ =>
        val prev = countAndSay(n-1)
        val counted: List[(Char, Int)] = countConsecutiveCharacters(prev.toCharArray.toList)
        counted.map({
          case (digit, numOfOccurrences) => s"$numOfOccurrences$digit"
        }).mkString
    }
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.countAndSay(4) == "1211")
  }
}
