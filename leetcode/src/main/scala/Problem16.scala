

object Problem16 {

  object Solution {

    def threeSumClosest(nums: Array[Int], target: Int): Int = {
      val sortedNums: Array[Int] = nums.sorted
      val sortedNumsWithIndices: Array[(Int, Int)] = sortedNums.zipWithIndex

      def findClosest(searchTarget: Int, blacklistIndices: Set[Int]): Int = {
        var left = 0
        var right = sortedNums.length - 1

        if(searchTarget < sortedNums.head){
          sortedNums(sortedNums.indices.diff(blacklistIndices.toSeq).head)
        }else if(searchTarget > sortedNums.last){
          sortedNums(sortedNums.indices.diff(blacklistIndices.toSeq).last)
        }else {
          var indexOfFound: Option[Int] = None
          while (left <= right && indexOfFound.isEmpty) {
            val mid = (left + right) / 2
            if (sortedNums(mid) < searchTarget) {
              left = mid + 1
            } else if (sortedNums(mid) > searchTarget) {
              right = mid - 1
            } else {
              indexOfFound = Some(mid)
            }
          }

          val candidates = Set(indexOfFound, Some(left), Some(right)).flatten
            .flatMap(x => Set(x - 1, x, x - 2, x + 1, x + 2))
            .filter(x => x >= 0 && x < sortedNums.length)
            .diff(blacklistIndices)
            .map(i => sortedNums(i))

          candidates.minBy(x => math.abs(x - searchTarget))
        }
      }

      (for {
        (ni, i) <- sortedNumsWithIndices
        (nj, j) <- sortedNumsWithIndices if j > i
      } yield {
        findClosest(target - ni - nj, Set(i, j)) + ni + nj
      }).minBy(x => math.abs(target - x))

    }
  }

  def main(args: Array[String]): Unit = {

    var nums = Array(-1, 2, 1, -4)
    var target = 1

    var res = Solution.threeSumClosest(nums, target)

    println(res)

    nums = Array(0, 0, 0)
    target = 1

    res = Solution.threeSumClosest(nums, target)

    println(res)

    nums = Array(-65, -46, -10, -79, -86, 39, 40, 62, 31, -40, -80, -20, -6, 8, 38, -33, 97, -99, -86, 8, 85, 57, 78, -92, 10, 5, 84, -15, 32, 11, -15, -5, -56, 86, 47, -78, 39, 88, -86, 24, -77, 52, -55, 16, 22, -57, -39, -16, -32, -2, -94, -43, 13, -49, 77, 96, 35, -46, -47, 10, -57, -73, 95, -22, -22, 5, -3, 81, 79, -15, -34, 41, -91, 26, -15, 72, 35, 100, 100, -89, -79, 70, 8, -99, -45, 75, -57, 15, 34, -16, 43, 54, -99, 39, -42, 87, -88, -69, 39, 15, 12, 29, 71, 48, -51, 20, -18, -37, 95, -81, -71, 22, 56, -87, 90, 78, -57, -37, -17, -64, 82, -28, -25, -83, 75, 21, 97, 35, 67, 12, 55, -91, -63, 4, -46, 15, -19, -60, 41, 29, -71, 26, 25, -85, -15, -81, -53, 48, 31, 28, 88, -71, 19, 83, 38, -42, -94, 42, 62, -43, 90, -81, -60, 56, -47, 34, -60, 73, -67, 72, -99, -46, -47, 10, 46, -86, -42)
    target = 220

    res = Solution.threeSumClosest(nums, target)

    println(res)
  }
}
