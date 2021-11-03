import Problem19.Solution.removeNthFromEnd

object Problem19 {
   class ListNode(_x: Int = 0, _next: ListNode = null) {
       var next: ListNode = _next
       var x: Int = _x

     override def toString: String = {
       if(next==null){
         s"$x"
       }else{
         s"$x->${next.toString}"
       }
     }
   }

  object Solution {
    import scala.annotation.tailrec

    @tailrec
    private def getSizeOfList(node: ListNode, size: Int = 0): Int = {
      if(node == null){
        size
      }else{
        getSizeOfList(node.next, size+1)
      }
    }

    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      val listSize: Int = getSizeOfList(head)
      val elementToRemoveIdx: Int = listSize - n
      if(elementToRemoveIdx==0){
        head.next
      }else{
        var i = 0
        var currNode = head
        while(i < elementToRemoveIdx-1){
          i+=1
          currNode = currNode.next
        }
        currNode.next = currNode.next.next
        head
      }
    }
  }

  def main(args: Array[String]): Unit = {
//    val head = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, new ListNode(5)))))
    val head = new ListNode(1,new ListNode(2))
    val res = removeNthFromEnd(head, 1)

    println(res)
  }
}
