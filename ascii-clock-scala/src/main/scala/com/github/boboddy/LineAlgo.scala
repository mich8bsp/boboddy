package com.github.boboddy

object LineAlgo {
  type Point = (Int, Int)

  def getPixelsOfLineBresenham(point1: Point, point2: Point): Seq[Point] = {
    val (x0, y0) = point1
    val (x1, y1) = point2

    if(math.abs(y1-y0) < math.abs(x1-x0)){
      if(x0 > x1){
        plotLineLow(x1, y1, x0, y0)
      }else{
        plotLineLow(x0, y0, x1, y1)
      }
    }else{
      if(y0 > y1){
        plotLineHigh(x1, y1, x0, y0)
      }else{
        plotLineHigh(x0, y0, x1, y1)
      }
    }
  }

  private def plotLineLow(x0: Int, y0: Int, x1: Int, y1: Int): Seq[Point] = {
    val dx = x1 - x0
    var dy = y1 - y0
    var yi = 1
    if(dy < 0){
      yi = -1
      dy = -dy
    }
    var D = (2 * dy) - dx
    var y = y0
    (x0 to x1).map(x => {
      val chosenPoint = (x, y)
      if(D > 0){
        y = y + yi
        D = D + (2 * (dy - dx))
      }else{
        D = D + 2*dy
      }
      chosenPoint
    })
  }

  private def plotLineHigh(x0: Int, y0: Int, x1: Int, y1: Int): Seq[Point] = {
    var dx = x1 - x0
    val dy = y1 - y0
    var xi = 1
    if(dx < 0){
      xi = -1
      dx = -dx
    }
    var D = (2 * dx) - dy
    var x = x0
    (y0 to y1).map(y => {
      val chosenPoint = (x, y)
      if(D > 0){
        x = x + xi
        D = D + (2 * (dx - dy))
      }else{
        D = D + 2*dx
      }
      chosenPoint
    })
  }

}
