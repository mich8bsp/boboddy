import WordleBot.GameConfig

import scala.io.Source

object Dictionary {
  private val dictionary: Seq[String] = Source.fromResource("words_alpha.txt").getLines().toList
  def wordleWords(implicit config: GameConfig): Seq[WordleWord] = dictionary.filter(_.length == config.wordLength)
    .map(WordleWord)
}

case class WordleWord(word: String){
  val charsDistribution: Map[Char, Int] = word.groupBy(identity)
    .view.mapValues(_.length)
    .toMap
}