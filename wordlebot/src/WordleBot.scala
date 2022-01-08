import scala.io.StdIn

object WordleBot {

  private def chooseWord(possibleSolutions: Seq[WordleWord]): WordleWord = {
    val totalCharOccurrences: Map[(Char, Int), Int] = possibleSolutions.map(_.charsDistribution)
      .map(_.map({
        case (c, n) => (c, n) -> 1
      }))
      .foldLeft(Map[(Char, Int), Int]())((a, b) => {
        (a.keySet ++ b.keySet).map(charWithNumOfOccurrencesInWord => charWithNumOfOccurrencesInWord -> {
          a.getOrElse(charWithNumOfOccurrencesInWord, 0) + b.getOrElse(charWithNumOfOccurrencesInWord, 0)
        }).toMap
      })

    def getScore(word: WordleWord): Int = {
      word.charsDistribution.map({
        case (c, n) => totalCharOccurrences.getOrElse((c, n), 0)
      }).sum
    }

    possibleSolutions.maxBy(getScore)
  }

  private def isValidSolution(solution: WordleWord, guess: String, output: String): Boolean = {
    def generateOutputForGuessAndSolution(guess: String, solution: String): String = {
      val solutionArr: Array[Char] = solution.toCharArray
      val exactMatches: Seq[(Char, Int)] = guess.toCharArray.zipWithIndex.filter({
        case (c, i) => solutionArr(i) == c
      })

      exactMatches.foreach(x => solutionArr(x._2) = '-')

      guess.toCharArray.zipWithIndex.map({
        case (c, i) => if(exactMatches.contains((c, i))){
          'g'
        }else if(solutionArr.contains(c)){
          solutionArr(solutionArr.indexOf(c)) = '-'
          'y'
        }else{
          '-'
        }
      }).mkString

    }

    generateOutputForGuessAndSolution(guess, solution.word) == output
  }

  def play(): Option[String] = {
    var possibleSolutions: Seq[WordleWord] = Dictionary.wordleWords
    var turn = 0
    var solution: Option[WordleWord] = None

    while(turn < 6 && solution.isEmpty){
      val guess: WordleWord = chooseWord(possibleSolutions)
      println(guess.word)
      val output = StdIn.readLine
      if(output == "X"){
        possibleSolutions = possibleSolutions.filterNot(_.word == guess.word)
      }else{
        possibleSolutions = possibleSolutions.filter(isValidSolution(_, guess.word, output))
        if(possibleSolutions.size == 1){
          solution = possibleSolutions.headOption
        }
        turn += 1
      }
    }

    solution.map(_.word)
  }

  def main(args: Array[String]): Unit = {
    play() match {
      case Some(x) => println(s"Solved! Word is $x")
      case None => println(s"Could not solve")
    }
  }
}