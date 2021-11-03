import scala.collection.mutable

object Problem22 {

  object Solution {

    private val cache: mutable.Map[Int, List[String]] = mutable.Map[Int, List[String]]()
    private val sumCombinationsCache: mutable.Map[Int, List[List[Int]]] = mutable.Map[Int, List[List[Int]]]()

    def getSumCombinations(n: Int): List[List[Int]] = n match {
      case 0 => List()
      case 1 => List(List(1))
      case _ =>
        (1 until n).flatMap(i => sumCombinationsCache.getOrElseUpdate(n-i, getSumCombinations(n-i)).map(r => i::r)).toList ++ List(List(n))
    }

    def generateParenthesis(n: Int): List[String] = n match {
      case 0 => List("")
      case _ =>
        cache.getOrElseUpdate(n, {
          val componentsOptions = getSumCombinations(n)
          componentsOptions.flatMap({
            case List(x) if x == n => generateParenthesis(n-1)
              .map(x => s"($x)")
            case components => components.map(i => generateParenthesis(i))
              .reduceLeft((prev, curr) => {
                for(x <- prev; y <- curr) yield s"$x$y"
              })
          }).distinct
        })
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.generateParenthesis(4))
  }
}
