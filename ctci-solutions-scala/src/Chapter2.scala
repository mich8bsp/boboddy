import scala.collection.mutable

object Chapter2 {

  class LinkedList[T](val data: T) {
    var next: Option[LinkedList[T]] = None

    def append(d: T): LinkedList[T] = {
      var prev = this
      while (prev.next.nonEmpty) {
        prev = prev.next.get
      }
      prev.next = Some(new LinkedList(d))
      this
    }

    def prepend(d: T): LinkedList[T] = {
      val newHead = new LinkedList[T](d)
      newHead.next = Some(this)
      newHead
    }

    def removeAt(idx: Int): Option[LinkedList[T]] = {
      var counter: Int = 0
      var prevOpt: Option[LinkedList[T]] = Some(this)
      if (idx == 0) {
        this.next
      } else if (idx < 0) {
        throw new IndexOutOfBoundsException()
      } else {
        while (counter < idx - 1) {
          counter += 1
          prevOpt = prevOpt.flatMap(_.next)
        }
        (prevOpt, prevOpt.flatMap(_.next)) match {
          case (Some(prev), Some(current)) => prev.next = current.next
            Some(this)
          case _ => throw new IndexOutOfBoundsException()
        }
      }
    }

    def remove(element: T): Option[LinkedList[T]] = {
      var prevOpt: Option[LinkedList[T]] = Some(this)
      if (this.data == element) {
        this.next
      } else {
        while (prevOpt.flatMap(_.next).map(_.data).exists(_ != element)) {
          prevOpt = prevOpt.flatMap(_.next)
        }
        (prevOpt, prevOpt.flatMap(_.next)) match {
          case (Some(prev), Some(current)) => prev.next = current.next
            Some(this)
          case _ => throw new NoSuchElementException()
        }
      }
    }

    def reverse: LinkedList[T] = {
      var pointer: Option[LinkedList[T]] = next
      var reversedPointer: LinkedList[T] = new LinkedList[T](data)

      while(pointer.nonEmpty){
        val newNodeInReversed: LinkedList[T] = new LinkedList[T](pointer.get.data)
        newNodeInReversed.next = Some(reversedPointer)
        reversedPointer = newNodeInReversed
        pointer = pointer.flatMap(_.next)
      }

      reversedPointer
    }

    def sameElements(other: LinkedList[T]): Boolean = {
      var thisPointer: Option[LinkedList[T]] = Some(this)
      var otherPointer: Option[LinkedList[T]] = Some(other)
      while(thisPointer.isDefined || otherPointer.isDefined){
        if(thisPointer.map(_.data) != otherPointer.map(_.data)){
          return false
        }else{
          thisPointer = thisPointer.flatMap(_.next)
          otherPointer = otherPointer.flatMap(_.next)
        }
      }
      true
    }

    def size: Int = {
      var s = 1
      var pointer: Option[LinkedList[T]] = next
      while(pointer.isDefined) {
        s += 1
        pointer = pointer.flatMap(_.next)
      }
      s
    }

    override def toString: String = {
      s"${data}${next.map(x => s"->$x").getOrElse("")}"
    }

  }

  object LinkedList {
    def fromString[T](str: String)(implicit nodeValueConverter: String => T): Option[LinkedList[T]] = {
      val nodesAsString: Array[String] = str.split("->")
      var head: Option[LinkedList[T]] = None
      var currentOpt: Option[LinkedList[T]] = head
      nodesAsString.foreach(nodeData => {
        val nextNode = new LinkedList[T](nodeValueConverter(nodeData))
        currentOpt match {
          case None =>
            currentOpt = Some(nextNode)
            head = currentOpt
          case Some(current) =>
            current.next = Some(nextNode)
            currentOpt = current.next
        }
      })

      head
    }
  }

  class DoubleLinkedList[T](val data: T) {
    var next: Option[DoubleLinkedList[T]] = None
    var prev: Option[DoubleLinkedList[T]] = None

    def append(d: T): DoubleLinkedList[T] = {
      var last = this
      while (last.next.nonEmpty) {
        last = last.next.get
      }
      val newNode = new DoubleLinkedList[T](d)
      newNode.prev = Some(last)
      last.next = Some(newNode)
      this
    }

    def prepend(d: T): DoubleLinkedList[T] = {
      val newHead = new DoubleLinkedList[T](d)
      newHead.next = Some(this)
      this.prev = Some(newHead)
      newHead
    }

    def removeAt(idx: Int): Option[DoubleLinkedList[T]] = {
      var counter: Int = 0
      var prevOpt: Option[DoubleLinkedList[T]] = Some(this)
      if (idx == 0) {
        this.next.foreach(_.prev = None)
        this.next
      } else if (idx < 0) {
        throw new IndexOutOfBoundsException()
      } else {
        while (counter < idx - 1) {
          counter += 1
          prevOpt = prevOpt.flatMap(_.next)
        }
        (prevOpt, prevOpt.flatMap(_.next)) match {
          case (Some(prev), Some(current)) =>
            prev.next = current.next
            current.next.foreach(_.prev = Some(prev))
            Some(this)
          case _ => throw new IndexOutOfBoundsException()
        }
      }
    }

    override def toString: String = {
      s"${data}${next.map(x => s"<->$x").getOrElse("")}"
    }
  }

  object DoubleLinkedList {
    def fromString(str: String): Option[DoubleLinkedList[String]] = {
      val nodesAsString: Array[String] = str.split("<->")
      var head: Option[DoubleLinkedList[String]] = None
      var currentOpt: Option[DoubleLinkedList[String]] = head
      nodesAsString.foreach(nodeData => {
        val nextNode = new DoubleLinkedList[String](nodeData)
        currentOpt match {
          case None =>
            currentOpt = Some(nextNode)
            head = currentOpt
          case Some(current) =>
            current.next = Some(nextNode)
            nextNode.prev = currentOpt
            currentOpt = current.next
        }
      })

      head
    }
  }

  // 2.1 removeDuplicates
  def removeDups[T](list: LinkedList[T]): Unit = {
    val encounteredNodes: mutable.Set[T] = mutable.Set[T]()

    encounteredNodes.add(list.data)
    var current: Option[LinkedList[T]] = Some(list)
    var next: Option[LinkedList[T]] = current.flatMap(_.next)

    while (next.isDefined) {
      if (encounteredNodes.contains(next.get.data)) {
        current.get.next = next.flatMap(_.next)
      } else {
        encounteredNodes.add(next.get.data)
        current = next
      }
      next = current.flatMap(_.next)
    }
  }

  // 2.2 Return kth to last
  def kthToLast[T](list: LinkedList[T], k: Int): Option[T] = {
    var pointer: Option[LinkedList[T]] = Some(list)
    var pointerPlusK: Option[LinkedList[T]] = pointer
    (0 until k - 1).foreach(_ => pointerPlusK = pointerPlusK.flatMap(_.next))
    pointerPlusK match {
      case None => None
      case _ =>
        pointerPlusK = pointerPlusK.flatMap(_.next)
        while (pointerPlusK.isDefined) {
          pointer = pointer.flatMap(_.next)
          pointerPlusK = pointerPlusK.flatMap(_.next)
        }
        pointer.map(_.data)
    }
  }

  //2.3 delete middle node
  def deleteMiddleNode[T](list: LinkedList[T], element: T): Unit = {
    list.remove(element)
  }

  //2.4 partition
  def partition(lst: LinkedList[Int], element: Int): LinkedList[Int] = {
    var lastElement: LinkedList[Int] = lst
    var elementsToCheck: Int = 0
    while (lastElement.next.nonEmpty) {
      lastElement = lastElement.next.get
      elementsToCheck += 1
    }
    var prevOpt: Option[LinkedList[Int]] = None
    var current: LinkedList[Int] = lst
    var head: LinkedList[Int] = lst
    while (elementsToCheck > 0) {
      if (current.data < element) {
        prevOpt = Some(current)
        current = current.next.get
      } else {
        val elementToMove = current
        current = current.next.get
        prevOpt match {
          case None => head = current
          case Some(prev) => prev.next = Some(current)
        }
        elementToMove.next = None
        lastElement.next = Some(elementToMove)
        lastElement = lastElement.next.get
      }
      elementsToCheck -= 1
    }

    head
  }

  //2.5 sum lists
  def sumLists(num1: LinkedList[Int], num2: LinkedList[Int]): LinkedList[Int] = {
    def innerSumLists(remainingDigits1: Option[LinkedList[Int]],
                      remainingDigits2: Option[LinkedList[Int]],
                      sumListHead: Option[LinkedList[Int]] = None,
                      sumListCurrent: Option[LinkedList[Int]] = None,
                      carry: Int = 0): LinkedList[Int] = {
      val digitFromFirst: Int = remainingDigits1.map(_.data).getOrElse(0)
      val digitFromSecond: Int = remainingDigits2.map(_.data).getOrElse(0)
      val sumOfDigitsWithCarry: Int = digitFromFirst + digitFromSecond + carry
      if (sumOfDigitsWithCarry > 0) {
        val newDigitInSum: LinkedList[Int] = sumListCurrent match {
          case None => new LinkedList[Int](sumOfDigitsWithCarry % 10)
          case Some(s) =>
            s.next = Some(new LinkedList[Int](sumOfDigitsWithCarry % 10))
            s.next.get
        }
        innerSumLists(
          remainingDigits1 = remainingDigits1.flatMap(_.next),
          remainingDigits2 = remainingDigits2.flatMap(_.next),
          sumListHead = sumListHead.orElse(Some(newDigitInSum)),
          sumListCurrent = Some(newDigitInSum),
          carry = sumOfDigitsWithCarry / 10)
      } else {
        sumListHead.getOrElse(new LinkedList[Int](0))
      }
    }

    innerSumLists(Some(num1), Some(num2))
  }

  //2.6 Palindrome
  def isPalindrome[T](lst: LinkedList[T]): Boolean = {
    lst.sameElements(lst.reverse)
  }

  //2.7 Intersection
  def getIntersectionNode[T](lst1: LinkedList[T], lst2: LinkedList[T]): Option[LinkedList[T]] = {
    val list1Size: Int = lst1.size //O(n)
    val list2Size: Int = lst2.size //O(n)
    var lst1Pointer: Option[LinkedList[T]] = Some(lst1)
    var lst2Pointer: Option[LinkedList[T]] = Some(lst2)
    if(list1Size > list2Size){
      (0 until list1Size - list2Size).foreach(_ => lst1Pointer = lst1Pointer.flatMap(_.next))
    }else if(list2Size > list1Size){
      (0 until list2Size - list1Size).foreach(_ => lst2Pointer = lst2Pointer.flatMap(_.next))
    }
    //at this point lst1Pointer and lst2Pointer point to the parts of their respective lists that have the same length
    //we can advance them together and check where they intersect
    while(lst1Pointer.isDefined && lst2Pointer.isDefined){
      if(lst1Pointer.get == lst2Pointer.get){
        return lst1Pointer
      }
      lst1Pointer = lst1Pointer.flatMap(_.next)
      lst2Pointer = lst2Pointer.flatMap(_.next)
    }
    None
  }

  //2.8 Loop Detection
  def getLoopStart[T](lst: LinkedList[T]): LinkedList[T] = {
    val step: LinkedList[T] => LinkedList[T] = x => x.next.get
    val doubleSpeedStep: LinkedList[T] => LinkedList[T] = x => x.next.flatMap(_.next).get

    var tortoise: LinkedList[T] = step(lst)
    var hare: LinkedList[T] = doubleSpeedStep(lst)
    while(tortoise != hare){
      tortoise = step(tortoise)
      hare = doubleSpeedStep(hare)
    }
    tortoise = lst
    while(tortoise != hare){
      tortoise = step(tortoise)
      hare = step(hare)
    }
    tortoise
  }

  def main(args: Array[String]): Unit = {
    implicit val impl = (str: String) => str.toInt
    val lst1: LinkedList[Int] = LinkedList.fromString[Int]("7->1->1->7").get
//    val lst2: LinkedList[Int] = LinkedList.fromString[Int]("5->9->2").get
//    println(sumLists(lst1, lst2))
    println(isPalindrome(lst1))
  }

}
