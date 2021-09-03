package com.github.boboddy

import com.github.boboddy.AsciiClock.CLOCK_SIZE
import com.github.boboddy.LineAlgo.Point

object Rendering {

  val EMPTY_CHAR = ' '
  val HOUR_SYMBOL = 'H'
  val MINUTE_SYMBOL = 'm'
  val SECOND_SYMBOL = 's'
  val CLOCK_FRAME_HORIZONTAL_SYMBOL = '-'
  val CLOCK_FRAME_VERTICAL_SYMBOL = '|'
  val CLOCK_FRAME_CORNER_SYMBOL = '□'
  val CLOCK_FRAME_CENTER_SYMBOL = 'X'

  def drawClock(
                 hoursHandPoints: Seq[Point],
                 minutesHandPoints: Seq[Point],
                 secondsHandPoints: Seq[Point]
               ): String = {
    val clock: Array[Array[Char]] = Array.fill(CLOCK_SIZE) {
      Array.fill(CLOCK_SIZE) {EMPTY_CHAR}
    }

    hoursHandPoints.foreach({
      case (i, j) => clock(CLOCK_SIZE - j - 1)(i) = HOUR_SYMBOL
    })
    minutesHandPoints.foreach({
      case (i, j) => clock(CLOCK_SIZE - j - 1)(i) = MINUTE_SYMBOL
    })
    secondsHandPoints.foreach({
      case (i, j) => clock(CLOCK_SIZE - j - 1)(i) = SECOND_SYMBOL
    })

    drawClockFrame(clock)

    clock.map(_.mkString("")).mkString("\n")

  }

  private def drawClockFrame(screen: Array[Array[Char]]): Unit = {
    screen(CLOCK_SIZE/2)(CLOCK_SIZE/2) = CLOCK_FRAME_CENTER_SYMBOL
    (0 until CLOCK_SIZE).foreach(i => {
      screen(0)(i) = CLOCK_FRAME_HORIZONTAL_SYMBOL
      screen(i)(0) = CLOCK_FRAME_VERTICAL_SYMBOL
      screen(CLOCK_SIZE - 1)(i) = CLOCK_FRAME_HORIZONTAL_SYMBOL
      screen(i)(CLOCK_SIZE - 1) = CLOCK_FRAME_VERTICAL_SYMBOL
    })
    screen(0)(0) = CLOCK_FRAME_CORNER_SYMBOL
    screen(CLOCK_SIZE - 1)(0) = CLOCK_FRAME_CORNER_SYMBOL
    screen(0)(CLOCK_SIZE - 1) = CLOCK_FRAME_CORNER_SYMBOL
    screen(CLOCK_SIZE - 1)(CLOCK_SIZE - 1) = CLOCK_FRAME_CORNER_SYMBOL
  }
}
