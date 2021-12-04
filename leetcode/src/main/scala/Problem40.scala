
object Problem40 {

  object Solution {

    import scala.collection.mutable

    def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
      val cache: mutable.Map[(Int, List[Int]), List[List[Int]]] = mutable.Map()

      def combinationSum2Inner(candidatesLeft: List[Int], sumLeft: Int): List[List[Int]] = {
        val possibleCandidates = candidatesLeft.filter(_ <= sumLeft)
        possibleCandidates.indices.map(candidateIdx => {
          val currentCandidate: Int = possibleCandidates(candidateIdx)
          if (currentCandidate == sumLeft) {
            List[List[Int]](List(currentCandidate))
          } else {
            val compliment: Int = sumLeft - currentCandidate
            val candidatesForCompliment: List[Int] = if (candidateIdx + 1 == possibleCandidates.length) {
              List[Int]()
            } else {
              possibleCandidates.slice(candidateIdx + 1, possibleCandidates.length)
            }
            val solutionsForCompliment = cache.getOrElseUpdate((compliment, candidatesForCompliment),
              combinationSum2Inner(candidatesForCompliment, compliment))

            solutionsForCompliment.map(_.prepended(currentCandidate))
          }
        }).foldLeft(List[List[Int]]())(_ ++ _)
          .distinctBy(_.groupBy(identity))
      }

      combinationSum2Inner(candidates.toList, target)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.combinationSum2(Array(10, 1, 2, 7, 6, 1, 5), 8))
    assert(Solution.combinationSum2(Array(10, 1, 2, 7, 6, 1, 5), 8) == List(List(1, 1, 6), List(1, 2, 5), List(1, 7), List(2, 6)))

  }
}
