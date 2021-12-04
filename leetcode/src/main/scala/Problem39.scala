
object Problem39 {

  object Solution {
    import scala.collection.mutable

    def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
      val cache: mutable.Map[Int, List[List[Int]]] = mutable.Map[Int, List[List[Int]]]()

      def combinationSumIter(sumLeft: Int): List[List[Int]] = {
        candidates.filter(_ <= sumLeft).map(currCandidate => {
          if (sumLeft == currCandidate) {
            List[List[Int]](List(currCandidate))
          } else {
            val compliment: Int = sumLeft - currCandidate
            val solutionsForCompliment = cache.getOrElseUpdate(compliment, combinationSumIter(compliment))
            solutionsForCompliment.map(_.prepended(currCandidate))
          }
        }).foldLeft(List[List[Int]]())(_ ++ _)
          .distinctBy(_.groupBy(identity))
//          .map(_.sorted)
      }

      combinationSumIter(target)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.combinationSum(Array(2, 3, 6, 7), 7))
    assert(Solution.combinationSum(Array(2, 3, 6, 7), 7) == List(List(2, 2, 3), List(7)))
    assert(Solution.combinationSum(Array(2, 3, 5), 8) == List(List(2, 2, 2, 2), List(2, 3, 3), List(3, 5)))
    assert(Solution.combinationSum(Array(2), 1) == List())
    assert(Solution.combinationSum(Array(1), 1) == List(List(1)))
    assert(Solution.combinationSum(Array(1), 2) == List(List(1, 1)))
  }
}
