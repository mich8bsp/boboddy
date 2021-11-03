import scala.collection.mutable

object Problem22 {

  object Solution {

    private val cache: mutable.Map[Int, List[String]] = mutable.Map[Int, List[String]]()

    def generateNumberComponents(n: Int): List[List[Int]] = n match {
      case 0 => List()
      case 1 => List(List(1))
      case _ =>
        (1 until n).flatMap(i => generateNumberComponents(n-i).map(r => i::r)).toList ++ List(List(n))
    }

    def generateParenthesis(n: Int): List[String] = n match {
      case 1 => cache.getOrElseUpdate(1, List("()"))
      case _ =>
        cache.getOrElseUpdate(n, {
          val componentsOptions = generateNumberComponents(n)
          componentsOptions.flatMap({
            case List(x) if x == n => cache.getOrElseUpdate(n-1, generateParenthesis(n-1))
              .map(x => s"($x)")
            case components => components.map(i => cache.getOrElseUpdate(i, generateParenthesis(i)))
              .reduceLeft((prev, curr) => {
                for{
                  x: String <- prev
                  y: String <- curr
                }yield{
                  s"$x$y"
                }
              })
          }).distinct
        })
    }
  }

  def main(args: Array[String]): Unit = {
//    println(Solution.generateNumberComponents(1))
//    println(Solution.generateNumberComponents(2))
//    println(Solution.generateNumberComponents(3))
//    println(Solution.generateNumberComponents(4))
//    println(Solution.generateNumberComponents(5))
//    println(Solution.generateNumberComponents(6))
//    println(Solution.generateNumberComponents(7))
//    println(Solution.generateNumberComponents(8))
    println(Solution.generateParenthesis(4))
  }
}
