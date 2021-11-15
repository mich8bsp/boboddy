object Problem31 {

  object Solution {
    def nextPermutation(nums: Array[Int]): Unit = {
      def isSortedDesc(arr: Array[Int]): Boolean = {
        var idx = 0
        while(idx < arr.length-1){
          if(arr(idx) >= arr(idx+1)){
            idx += 1
          }else{
            return false
          }
        }
        true
      }

      def reverseArray(arr: Array[Int], start: Int, end: Int): Unit = {
        var left = start
        var right = end-1
        while (left < right){
          val tmp = arr(left)
          arr(left) = arr(right)
          arr(right) = tmp
          left += 1
          right -= 1
        }
      }

      if(nums.length>1){
        if(isSortedDesc(nums)){
          reverseArray(nums, 0, nums.length)
        }else{
          var leftIdx = nums.length-2
          while(leftIdx >= 0 && nums(leftIdx) >= nums(leftIdx+1) ){
            leftIdx -= 1
          }
          if(leftIdx>=0){
            var rightIdx = nums.length - 1
            while(nums(rightIdx) <= nums(leftIdx)){
              rightIdx -= 1
            }
            val tmp = nums(leftIdx)
            nums(leftIdx) = nums(rightIdx)
            nums(rightIdx) = tmp
          }
          reverseArray(nums, leftIdx+1, nums.length)

        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    var nums = Array(1,2,3)

    def run() = {
      Solution.nextPermutation(nums)
      println(nums.mkString("Array(", ", ", ")"))
    }

//    run() // 1,3,2
//
//    nums = Array(3,2,1)
//    run() // 1,2,3
//
//    nums = Array(1,1,5)
//    run() // 1,5,1
//
//    nums = Array(1)
//    run() //1

    nums = Array(1,1,2,2)
    run() // 1,2,1,2

    nums = Array(1,3,2)
    run() //2,1,3
  }
}
