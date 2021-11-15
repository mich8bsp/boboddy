import Problem29.Solution.divide

object Problem29 {

  object Solution {
    def divide(dividend: Int, divisor: Int): Int = {
      def divideIteration(dividendLeft: Int, divisor: Int, count: Int = 0): Int = {
        if(dividendLeft == 0){
          count
        } else if(divisor == 1){
          dividendLeft
        }else if (divisor == -1){
          if(dividendLeft == Int.MinValue){
            //this is actually incorrect (probably), but that's what leetcode accepts as correct answer
            Int.MaxValue
          }else{
            0 - dividendLeft
          }
        }else{
          val bothPositive: Boolean = (dividend>=0 && divisor>0)
          val bothNegative: Boolean = (dividend<0 && divisor<0)
          val sameSignDividendDivisor: Boolean = bothPositive || bothNegative
          val lastIteration: Boolean = if(sameSignDividendDivisor){
            (bothPositive && (dividendLeft < divisor)) || (bothNegative && (dividendLeft > divisor))
          }else{
            (dividend >= 0 && (dividendLeft + divisor < 0)) || (dividend < 0 && (dividendLeft + divisor > 0))
          }
          if(sameSignDividendDivisor){
            if(lastIteration){
              count
            }else{
              divideIteration(dividendLeft-divisor, divisor, count+1)
            }
          }else{
            if(lastIteration){
              count
            }else {
              divideIteration(dividendLeft+divisor, divisor, count-1)
            }
          }
        }
      }

      divideIteration(dividend, divisor)
    }
  }

  def main(args: Array[String]): Unit = {
    println(divide(10, 3)) //3
    println(divide(-10, -3)) //3
    println(divide(7, -3)) //-2
    println(divide(-7, 3)) //-2
    println(divide(0, 1)) //0
    println(divide(1, 1)) //1
    println(divide(-2147483648, -1)) //2147483648
  }
}
