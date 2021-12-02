import scala.io.Source

object Rosalind {

  private val baseToComplementary: Map[Char, Char] = Map(
    'A' -> 'T',
    'T' -> 'A',
    'G' -> 'C',
    'C' -> 'G'
  )

  def countBases(dnaSequence: String): Map[Char, Int] = {
    Map[Char, Int](
      'A' -> dnaSequence.count(_ == 'A'),
      'C' -> dnaSequence.count(_ == 'C'),
      'G' -> dnaSequence.count(_ == 'G'),
      'T' -> dnaSequence.count(_ == 'T')
    )
  }

  def transcribeDNAtoRNA(dnaSequence: String): String = dnaSequence.replaceAll("T", "U")

  def strandCompliment(dnaSequence: String): String = {
    dnaSequence.reverse.filter(x => baseToComplementary.keySet.contains(x))
      .map(x => baseToComplementary(x))
  }

  def main(args: Array[String]): Unit = {
    val dnaSequence: String = Source.fromResource("rosalind_revc.txt").mkString
    println(strandCompliment(dnaSequence))
  }
}
