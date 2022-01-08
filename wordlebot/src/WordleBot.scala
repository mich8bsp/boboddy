import scala.io.StdIn

object WordleBot {
  val VALID_GUESS_EVALUATION_CHARS = Set('g', 'y', '-')
  val INVALID_GUESS_EVALUATION = 'X'

  private def combineCounts[T](a: Map[T, Int], b: Map[T, Int]): Map[T, Int] = (a.keySet ++ b.keySet)
    .map(k => k -> (a.getOrElse(k, 0) + b.getOrElse(k, 0)))
    .toMap

  private def chooseWord(possibleSolutions: Seq[WordleWord]): WordleWord = {
    val totalCharOccurrences: Map[(Char, Int), Int] = possibleSolutions.map(_.charsDistribution)
      .map(_.map(_ -> 1))
      .foldLeft(Map[(Char, Int), Int]())(combineCounts)

    def getScore(word: WordleWord): Int = {
      word.charsDistribution.map(occur => totalCharOccurrences.getOrElse(occur, 0)).sum
    }

    possibleSolutions.maxBy(getScore)
  }

  private def isValidSolution(solution: WordleWord, guess: String, output: String): Boolean = {
    def generateOutputForGuessAndSolution(guess: String, solution: String): String = {
      val solutionArr: Array[Char] = solution.toCharArray
      def markSolutionChecked(idx: Int): Unit = solutionArr(idx) = '-'

      val exactMatches: Seq[(Char, Int)] = guess.toCharArray.zipWithIndex.filter({
        case (c, i) => solutionArr(i) == c
      })

      exactMatches.map(_._2).foreach(markSolutionChecked)

      guess.toCharArray.zipWithIndex.map({
        case (c, i) => if(exactMatches.contains((c, i))){
          'g'
        }else if(solutionArr.contains(c)){
          markSolutionChecked(solutionArr.indexOf(c))
          'y'
        }else{
          '-'
        }
      }).mkString

    }

    generateOutputForGuessAndSolution(guess, solution.word) == output
  }

  def play()(implicit config: GameConfig): Option[String] = {
    var possibleSolutions: Seq[WordleWord] = Dictionary.wordleWords
    var turn = 0
    var solution: Option[WordleWord] = None

    while(turn < config.maxNumOfTurns && solution.isEmpty){
      val guess: WordleWord = chooseWord(possibleSolutions)
      println(guess.word)
      val output = StdIn.readLine
      output.toCharArray.toSet match {
        case x if x.subsetOf(VALID_GUESS_EVALUATION_CHARS) && output.length == config.wordLength =>
          possibleSolutions = possibleSolutions.filter(isValidSolution(_, guess.word, output))
          if(possibleSolutions.size == 1){
            solution = possibleSolutions.headOption
          }
          turn += 1
        case o if o == Set(INVALID_GUESS_EVALUATION) =>
          possibleSolutions = possibleSolutions.filterNot(_.word == guess.word)
        case _ =>
          println("invalid output")
      }
    }

    solution.map(_.word)
  }

  case class GameConfig(wordLength: Int, maxNumOfTurns: Int)

  def main(args: Array[String]): Unit = {
    implicit val config: GameConfig = GameConfig(wordLength = 5, maxNumOfTurns = 6)
    play() match {
      case Some(x) => println(s"Solved! Word is $x")
      case None => println(s"Could not solve")
    }
  }
}