import scala.collection.mutable
import scala.util.Random

object Chapter1 {

  def countStringChars(str: String): mutable.Map[Char, Int] = {
    val charsCount: mutable.Map[Char, Int] = mutable.Map[Char, Int]()
    str.foreach(c => {
      charsCount.updateWith(c)({
        case None => Some(1)
        case Some(prevCount) => Some(prevCount+1)
      })
    })

    charsCount
  }

  // 1.1 Is Unique
  def isUnique(str: String): Boolean = {
    //v1
    val charsInString: mutable.Set[Char] = mutable.Set[Char]()
    str.foreach(c => {
      if(charsInString.contains(c)){
        return false
      }else{
        charsInString.add(c)
      }
    })
    true

    //v2
    val sortedStr: String = str.sorted
    var prev: Option[Char] = None
    sortedStr.foreach(c => prev match {
      case Some(prevC) if c == prevC => return false
      case _ => prev = Some(c)
    })
    true
  }

  // 1.2 Check Permutation
  def checkPermutation(str1: String, str2: String): Boolean = {
    countStringChars(str1) == countStringChars(str2)
  }

  //1.3 URLify
  def urlify(str: mutable.Seq[Char], strTrueLength: Int): Unit = {
    var endPointer: Int = str.length - 1
    var iterPointer: Int = strTrueLength - 1
    while(iterPointer >= 0 && endPointer > iterPointer){
      str(iterPointer) match {
        case ' ' =>
          str(endPointer) = '0'
          str(endPointer-1) = '2'
          str(endPointer-2) = '%'
          iterPointer -= 1
          endPointer -= 3
        case c =>
          str(endPointer) = c
          iterPointer -= 1
          endPointer -= 1
      }
    }
  }

  //1.4 Palindrome Permutation
  def palindromePermutation(str: String): Boolean = {
    val strForPermutationCheck: String = str.toLowerCase.filter(_.isLetter)
    val charCounts: mutable.Map[Char, Int] = countStringChars(strForPermutationCheck)
    if(strForPermutationCheck.length % 2 == 1){
      //odd sized string is a palindrome if all characters appear on both sides of the palindrome, except for once character
      //which appears an odd number of times in the middle of the palindrome
      charCounts.count(_._2 % 2 == 0) == charCounts.size - 1
    }else{
      //even sized string is a palindrome if all characters appear on both sides of the palindrome
      charCounts.forall(_._2 % 2 == 0)
    }
  }

  //1.5 One Away
  def oneAway(str1: String, str2: String): Boolean = {
    if(str1 == str2){
      true
    }else if(math.abs(str1.length - str2.length) > 1){
      false
    }else{
      var str1Pointer = 0
      var str2Pointer = 0
      var changesEncountered = 0
      while(str1Pointer < str1.length && str2Pointer < str2.length){
        if(str1(str1Pointer) == str2(str2Pointer)){
          str1Pointer += 1
          str2Pointer += 1
        }else{
          changesEncountered += 1
          val str1SizeLeft = str1.length - str1Pointer
          val str2SizeLeft = str2.length - str2Pointer
          if(str1SizeLeft < str2SizeLeft){
            str2Pointer += 1
          }else if (str1SizeLeft > str2SizeLeft){
            str1Pointer += 1
          }else{
            str1Pointer += 1
            str2Pointer += 1
          }
        }
      }
      changesEncountered <= 1
    }
  }

  //1.6 String compression
  def stringCompression(str: String): String = {
    val originalLength: Int = str.length
    val encodingBuilder: StringBuilder = new StringBuilder()
    var prevCharWithCount: Option[(Char, Int)] = None
    str.foreach(c => prevCharWithCount match {
      case None => prevCharWithCount = Some((c, 1))
      case Some((prevC, prevCCount)) => if(c == prevC){
        prevCharWithCount = Some((prevC, prevCCount+1))
      }else{
        encodingBuilder.append(s"${prevC}${prevCCount}")
        prevCharWithCount = Some((c, 1))
        if(encodingBuilder.size >= originalLength){
          return str
        }
      }
    })
    prevCharWithCount.foreach({
      case (prevC, prevCCount) => encodingBuilder.append(s"$prevC$prevCCount")
    })
    if(encodingBuilder.size >= originalLength){
      str
    }else{
      encodingBuilder.toString()
    }
  }

  //1.7 Rotate Matrix

  type Pixel = (Byte, Byte, Byte, Byte) //RGBA
  def rotateMatrix(image: mutable.Buffer[mutable.Buffer[Pixel]]): Unit = {
    val N = image.length

    def rotateFrameAtDepth(depth: Int): Unit = {
      val frameSize = N - depth * 2
      if(frameSize>1){
        (0 until frameSize-1).foreach(offsetFromCorner => {
          // frame top part idx (depth)(depth + offsetFromCorner)
          // frame right part idx (depth + offsetFromCorner)(N - depth - 1)
          // frame bottom part idx (N - depth - 1)(N - depth - 1 - offsetFromCorner)
          // frame left part idx (N - depth - 1 - offsetFromCorner)(depth)

          val oldTop: Pixel = image(depth)(depth + offsetFromCorner)
          val oldRight: Pixel = image(depth + offsetFromCorner)(N - depth - 1)
          val oldBottom: Pixel = image(N - depth - 1)(N - depth - 1 - offsetFromCorner)
          val oldLeft: Pixel = image(N - depth - 1 - offsetFromCorner)(depth)

          image(depth)(depth + offsetFromCorner) = oldLeft
          image(depth + offsetFromCorner)(N - depth - 1) = oldTop
          image(N - depth - 1)(N - depth - 1 - offsetFromCorner) = oldRight
          image(N - depth - 1 - offsetFromCorner)(depth) = oldBottom
        })
      }
    }

    (0 until N/2).foreach(d => rotateFrameAtDepth(d))
  }

  //1.8 Zero Matrix
  def zeroMatrix(matrix: mutable.Buffer[mutable.Buffer[Int]]): Unit ={
    val rowsToZero: mutable.Set[Int] = mutable.Set[Int]()
    val colsToZero: mutable.Set[Int] = mutable.Set[Int]()

    matrix.indices.foreach(i => {
      matrix(i).indices.foreach(j => {
        if(matrix(i)(j) == 0){
          rowsToZero.add(i)
          colsToZero.add(j)
        }
      })
    })

    matrix.indices.foreach(i => {
      if(rowsToZero.contains(i)){
        matrix(i) = mutable.Buffer.from[Int](matrix(i).indices.map(_ => 0))
      }else{
        colsToZero.foreach(j => {
          matrix(i)(j) = 0
        })
      }
    })
  }

  //1.9 String Rotation
  def stringRotation(s1: String, s2: String): Boolean = {
    (s1 + s1).contains(s2)
  }

  def main(args: Array[String]): Unit = {
    val matrix = mutable.Buffer[mutable.Buffer[Int]]()
    val N = 3
    val M = 4
    (0 until N).foreach(i => {
      matrix.append(mutable.Buffer[Int]())
      (0 until M).foreach(_ => matrix(i).append(Random.nextInt(10)))
    })

    println(matrix)
    zeroMatrix(matrix)
    println(matrix)
  }
}
