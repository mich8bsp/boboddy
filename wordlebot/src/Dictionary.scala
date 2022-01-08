import scala.io.Source

object Dictionary {
  private val dictionary: Seq[String] = Source.fromResource("words_alpha.txt").getLines().toList
  val wordleWords: Seq[WordleWord] = dictionary.filter(_.length == 5).map(WordleWord)
}

case class WordleWord(word: String){
  val charsDistribution: Map[Char, Int] = word.groupBy(identity)
    .view.mapValues(_.length)
    .toMap
}