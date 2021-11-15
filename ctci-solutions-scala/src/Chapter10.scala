import scala.collection.mutable

object Chapter10 {

  //10.1 merge two sorted arrays
  def sortedMerge(sortedA: Array[Int], sortedB: Array[Int]): Unit = {
    val arrANumElements: Int = sortedA.length - sortedB.length
    val arrBNumElements: Int = sortedB.length
    val buffer: mutable.Queue[Int] = mutable.Queue[Int]()
    var arrAIdx: Int = 0
    var arrBIdx: Int = 0

    while(arrAIdx < sortedA.length || arrBIdx < sortedB.length || buffer.nonEmpty){
      val currA: Option[Int] = if(arrAIdx < arrANumElements) Some(sortedA(arrAIdx)) else None
      val currB: Option[Int] = if(arrBIdx < arrBNumElements) Some(sortedB(arrBIdx)) else None
      val currFromBuffer: Option[Int] = buffer.headOption

      (currA, currB, currFromBuffer) match {
        case (_, None, None) =>
        case (None, Some(b), None) =>
          sortedA(arrAIdx) = b
          arrAIdx += 1
          arrBIdx += 1
        case (None, None, Some(_)) =>
          sortedA(arrAIdx) = buffer.dequeue()
          arrAIdx += 1
        case (Some(a), Some(b), None) =>
          if(a<=b){
            arrAIdx += 1
          }else{
            buffer.enqueue(a)
            sortedA(arrAIdx) = b
            arrAIdx += 1
            arrBIdx += 1
          }
        case (None, Some(b), Some(c)) =>
          if(b<=c){
            sortedA(arrAIdx) = b
            arrAIdx += 1
            arrBIdx += 1
          }else{
            sortedA(arrAIdx) = buffer.dequeue()
            arrAIdx += 1
          }
        case (Some(a), Some(b), Some(c)) =>
          if(a <= b && a <= c){
            arrAIdx += 1
          }else if(b < a && b <= c){
            buffer.enqueue(a)
            sortedA(arrAIdx) = b
            arrAIdx += 1
            arrBIdx += 1
          }else {
            buffer.enqueue(a)
            sortedA(arrAIdx) = buffer.dequeue()
            arrAIdx += 1
          }
      }
    }
  }

  //10.2
  def groupAnagrams(arr: Array[String]): Array[String] ={
    val groupedAnagrams: Map[String, Array[String]] = arr.groupBy(_.sorted)
    groupedAnagrams.values.flatten.toArray
  }

  //10.3
  def searchInRotated(rotatedArray: Array[Int], target: Int): Option[Int] = {
    def innerSearchInRotated(leftIdx: Int, rightIdx: Int): Option[Int] = {
      val midIdx = (leftIdx + rightIdx) / 2
      if(rotatedArray(midIdx) == target){
        Some(midIdx)
      }else if(rightIdx < leftIdx){
        None
      }else{
        if(rotatedArray(leftIdx) < rotatedArray(midIdx)){
          //left side is not rotated
          if(target < rotatedArray(midIdx) && target >= rotatedArray(leftIdx)){
            innerSearchInRotated(leftIdx, midIdx-1)
          }else{
            innerSearchInRotated(midIdx+1, rightIdx)
          }
        }else if(rotatedArray(leftIdx) > rotatedArray(midIdx)){
          //right side is not rotated
          if(target > rotatedArray(midIdx) && target <= rotatedArray(rightIdx)){
            innerSearchInRotated(midIdx+1, rightIdx)
          }else{
            innerSearchInRotated(leftIdx, midIdx-1)
          }
        }else {
          if(rotatedArray(midIdx) != rotatedArray(rightIdx)){
            innerSearchInRotated(midIdx+1, rightIdx)
          }else{
            innerSearchInRotated(leftIdx, midIdx-1).orElse(
              innerSearchInRotated(midIdx+1, rightIdx)
            )
          }
        }

      }
    }

    innerSearchInRotated(0, rotatedArray.length - 1)
  }

  class Listy(arr: Array[Int]){
    def getAt(idx: Int): Int = if(idx >= arr.length){
      -1
    }else{
      arr(idx)
    }
  }
  //10.4
  def sortedSearchNoSize(listy: Listy, target: Int): Int = {
    var idx = 1
    var currElement = listy.getAt(idx)
    while(currElement >= 0 && currElement < target){
      idx *= 2
      currElement = listy.getAt(idx)
    }
    if(currElement == target){
      idx
    }else{
      val lastValidIdx: Int = idx / 2
      var left: Int = lastValidIdx
      var right: Int = idx
      while(left <= right){
        val mid = (left + right)/2
        val midElement = listy.getAt(mid)
        if(midElement == target){
          return mid
        }else if(midElement < target){
          left = mid + 1
        }else{
          right = mid - 1
        }
      }
    }

    -1
  }

  //10.5 sparse search
  def sparseSearch(arr: Array[String], target: String): Int ={
    val nonEmptyIndices: Array[Int] = arr.zipWithIndex.filter(_._1.nonEmpty).map(_._2)

    if(nonEmptyIndices.isEmpty){
      -1
    }else{
      var left = 0
      var right = nonEmptyIndices.length-1

      while(left <= right){
        val mid = (left + right) / 2
        val midElement = arr(nonEmptyIndices(mid))
        if(midElement == target){
          return nonEmptyIndices(mid)
        }else if(midElement < target){
          left = mid + 1
        }else {
          right = mid - 1
        }
      }
      -1
    }

  }




  def main(args: Array[String]): Unit = {
    println(sparseSearch(Array("at", "", "", "", "ball", "", "", "car", "", "", "dad", "", ""), "ball"))
  }
}
