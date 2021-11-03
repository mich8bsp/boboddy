

object Problem20 {

  object Solution{
    import scala.collection.mutable

    def isValid(s: String): Boolean = {
      val stack: mutable.Stack[Char] = mutable.Stack[Char]()
      s.toCharArray.foreach(c => {
        if(c == '(' || c == '[' || c == '{'){
          stack.push(c)
        }else{
          if(stack.isEmpty){
            return false
          }else{
            (stack.pop(), c) match {
              case ('(', ')') | ('[', ']') | ('{', '}') =>
              case _ => return false
            }
          }
        }
      })

      stack.isEmpty
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.isValid("()"))
    println(Solution.isValid("()[]{}"))
    println(Solution.isValid("(]"))
  }
}
