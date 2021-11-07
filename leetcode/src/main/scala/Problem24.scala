import Problem24.Solution.swapPairs

object Problem24 {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  object Solution {

    def swapPairs(head: ListNode): ListNode = {
      if(head == null || head.next == null){
        head
      }else{
        val newHead: ListNode = head.next
        var first: ListNode = head
        var second: ListNode = head.next
        var secondFromPreviousPair: ListNode = null
        while(first != null && second != null){
          if(secondFromPreviousPair != null){
            secondFromPreviousPair.next = second
          }
          first.next = second.next
          second.next = first
          secondFromPreviousPair = first
          first = first.next
          second = if(first!=null) first.next else null
        }

        newHead
      }
    }



  }

  def main(args: Array[String]): Unit = {
    val lst = new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4))))
    val res = swapPairs(lst)
    println(res)
  }
}
