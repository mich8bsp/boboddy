object Problem12 {

  object Solution {

    def intToRoman(num: Int): String = {
      val denominationToLetter = Map[Int, String](
        1 -> "I",
        5 -> "V",
        10 -> "X",
        50 -> "L",
        100 -> "C",
        500 -> "D",
        1000 -> "M"
      )
      def innerIntToRoman(currNum: Int, denomination: Int): String = {
        lazy val currDenLetter: String = denominationToLetter(denomination)
        lazy val nextDenLetter: String = denominationToLetter(denomination * 10)
        lazy val midDenLetter: String = denominationToLetter(denomination * 5)
        currNum match {
          case 0 => ""
          case _ if currNum >= 1 && currNum <= 3 => currDenLetter * currNum
          case 4 => currDenLetter + midDenLetter
          case 5 => midDenLetter
          case _ if currNum >= 6 && currNum <= 8 => midDenLetter + (currDenLetter * (currNum - 5))
          case 9 => currDenLetter + nextDenLetter
        }
      }

      val maxPower = 3
      (maxPower to 0 by -1).map(power => {
        val denominator = math.pow(10, power).toInt
        innerIntToRoman((num % (denominator * 10)) / denominator, denominator)
      }).reduce(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.intToRoman(3) == "III")
    assert(Solution.intToRoman(4) == "IV")
    assert(Solution.intToRoman(9) == "IX")
    assert(Solution.intToRoman(58) == "LVIII")
    assert(Solution.intToRoman(1994) == "MCMXCIV")
  }
}
