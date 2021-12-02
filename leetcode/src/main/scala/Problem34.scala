object Problem34 {

  object Solution {

    def findSequenceChangeFromEnd(nums: Array[Int], start: Int, end: Int): Int = {
      val target = nums(end)
      var leftIdx: Int = start
      var rightIdx: Int = end
      while (leftIdx < rightIdx - 1) {
        val midIdx = (leftIdx + rightIdx) / 2
        if (nums(midIdx) < target) {
          leftIdx = midIdx
        } else {
          rightIdx = midIdx
        }
      }
      if (nums(leftIdx) < target && nums(rightIdx) == target) {
        rightIdx
      } else {
        -1
      }
    }

    def findSequenceChangeFromStart(nums: Array[Int], start: Int, end: Int): Int = {
      val target = nums(start)
      var leftIdx: Int = start
      var rightIdx: Int = end
      while (leftIdx < rightIdx - 1) {
        val midIdx = (leftIdx + rightIdx) / 2
        if (nums(midIdx) > target) {
          rightIdx = midIdx
        } else {
          leftIdx = midIdx
        }
      }
      if (nums(leftIdx) == target && nums(rightIdx) > target) {
        leftIdx
      } else {
        -1
      }
    }

    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
      if (nums.isEmpty || nums.head > target || nums.last < target) {
        Array(-1, -1)
      } else {
        var leftIdx: Int = 0
        var rightIdx: Int = nums.length - 1
        var foundLeftIdx: Int = if (nums.head == target) 0 else -1
        var foundRightIdx: Int = if (nums.last == target) nums.length - 1 else -1
        while (leftIdx < rightIdx - 1 && (foundLeftIdx < 0 || foundRightIdx < 0)) {
          val midIdx = (leftIdx + rightIdx) / 2
          if (nums(midIdx) < target) {
            leftIdx = midIdx
          } else if (nums(midIdx) > target) {
            rightIdx = midIdx
          } else {
            if (foundLeftIdx < 0) {
              foundLeftIdx = findSequenceChangeFromEnd(nums, leftIdx, midIdx)
            }
            if (foundRightIdx < 0) {
              foundRightIdx = findSequenceChangeFromStart(nums, midIdx, rightIdx)
            }
          }
        }
        if (leftIdx == rightIdx - 1 && (foundLeftIdx < 0 || foundRightIdx < 0)) {
          (nums(leftIdx), nums(rightIdx)) match {
            case (left, right) if left == target && right > target =>
              foundRightIdx = leftIdx
            case (left, right) if left < target && right == target =>
              foundLeftIdx = rightIdx
            case _ =>
          }
        }
        Array(foundLeftIdx, foundRightIdx)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.searchRange(Array(1, 3), 1) sameElements Array(0, 0))
    assert(Solution.searchRange(Array(5, 7, 7, 8, 8, 10), 8) sameElements Array(3, 4))
    assert(Solution.searchRange(Array(5, 7, 7, 8, 8, 10), 6) sameElements Array(-1, -1))
    assert(Solution.searchRange(Array(), 0) sameElements Array(-1, -1))

  }
}
