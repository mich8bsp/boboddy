
object Problem17 {

  object Solution {

    private val digitMapping: Map[Int, Set[Char]] = (0 to 9).map((d: Int) => {
      val letters: Set[Char] = d match {
        case 0 | 1 => Set[Char]()
        case 2 => Set('a', 'b', 'c')
        case 3 => Set('d', 'e', 'f')
        case 4 => Set('g', 'h', 'i')
        case 5 => Set('j', 'k', 'l')
        case 6 => Set('m', 'n', 'o')
        case 7 => Set('p', 'q', 'r', 's')
        case 8 => Set('t', 'u', 'v')
        case 9 => Set('w', 'x', 'y', 'z')
      }
      d -> letters
    }).toMap


    def letterCombinations(digits: String): List[String] = {
      var combinations: Seq[String] = Seq("")
      digits.map(_.asDigit).foreach(d => {
        val chars: Set[Char] = digitMapping(d)
        combinations = for {
          combination <- combinations
          c <- chars
        }yield {
          combination + c
        }
      })

      combinations.filter(_.nonEmpty).toList
    }
  }


  def main(args: Array[String]): Unit = {
    var digits = "23"
    var combinations = Solution.letterCombinations(digits)
    println(combinations.mkString(", "))

    digits = ""
    combinations = Solution.letterCombinations(digits)
    println(combinations.mkString(", "))

    digits = "2"
    combinations = Solution.letterCombinations(digits)
    println(combinations.mkString(", "))

  }
}
