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
      def digitToRoman(digit: Int, denomination: Int): String = {
        lazy val currDenLetter: String = denominationToLetter(denomination)
        lazy val nextDenLetter: String = denominationToLetter(denomination * 10)
        lazy val midDenLetter: String = denominationToLetter(denomination * 5)
        digit match {
          case 0 => ""
          case _ if digit >= 1 && digit <= 3 => currDenLetter * digit
          case _ if digit >= 4 && digit <= 8 =>
            (currDenLetter * math.max(5 - digit, 0)) +
              midDenLetter +
              (currDenLetter * math.max(digit - 5, 0))
          case 9 => currDenLetter + nextDenLetter
        }
      }

      val maxPower = 3
      (maxPower to 0 by -1).map(power => {
        val denominator = math.pow(10, power).toInt
        digitToRoman((num % (denominator * 10)) / denominator, denominator)
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
