package com.github.boboddy

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.Date
import java.util.concurrent.TimeUnit

object AsciiClock {
  import LineAlgo.Point
  val WIDGET_SIZE = 31
  val EMPTY_CHAR = ' '
  val HOUR_SYMBOL = 'H'
  val MINUTE_SYMBOL = 'm'
  val SECOND_SYMBOL = 's'
  val CLOCK_FRAME_HORIZONTAL_SYMBOL = '-'
  val CLOCK_FRAME_VERTICAL_SYMBOL = '|'
  val CLOCK_FRAME_CORNER_SYMBOL = '□'
  val CLOCK_FRAME_CENTER_SYMBOL = 'X'

  def getAsciiTimeNow(): String = {
    val Array(hours, minutes, seconds) = new SimpleDateFormat("HH:mm:ss").format(Date.from(Instant.now))
      .split(":").map(_.toInt)

    val hoursAngleDegrees: Double = getAngleOf(hours, TimeUnit.HOURS)
    val minutesAngleDegrees: Double = getAngleOf(minutes, TimeUnit.MINUTES)
    val secondsAngleDegrees: Double = getAngleOf(seconds, TimeUnit.SECONDS)

    implicit val widgetScreen: Array[Array[Char]] = Array.fill(WIDGET_SIZE) {
      Array.fill(WIDGET_SIZE) {EMPTY_CHAR}
    }

    drawClockHand(hoursAngleDegrees, HOUR_SYMBOL, handSizeProportion = 0.5)
    drawClockHand(minutesAngleDegrees, MINUTE_SYMBOL)
    drawClockHand(secondsAngleDegrees, SECOND_SYMBOL)
    drawClockFrame()

    widgetScreen.map(_.mkString("")).mkString("\n")
  }

  private def getAngleOf(timeQuant: Int, timeUnit: TimeUnit): Double = {
    val fraction: Double = timeUnit match {
      case TimeUnit.HOURS => (timeQuant % 12).toDouble / 12D
      case TimeUnit.MINUTES | TimeUnit.SECONDS => timeQuant.toDouble / 60D
      case _ => throw new Exception(s"Unsupported time unit ${timeUnit}")
    }

    fraction * 360D
  }

  private def drawClockHand(angleOfHandDeg: Double, handSymbol: Char, handSizeProportion: Double = 1D)
                           (implicit screen: Array[Array[Char]]): Unit = {
    val halfClockSize: Int = WIDGET_SIZE / 2
    val handSize: Double = halfClockSize * handSizeProportion

    val angleOfHandDegCartesian: Double = (90 + (360 - angleOfHandDeg)) % 360
    val angleOfHandRadCartesian: Double = math.toRadians(angleOfHandDegCartesian)
    val handProjX: Int = (handSize * math.cos(angleOfHandRadCartesian)).toInt
    val handProjY: Int = (handSize * math.sin(angleOfHandRadCartesian)).toInt

    val handStartPoint: Point = (halfClockSize, halfClockSize)
    val handEndPoint: Point = (halfClockSize + handProjX, halfClockSize + handProjY)

    val pixelsToDraw: Seq[Point] = LineAlgo.getPixelsOfLineBresenham(handStartPoint, handEndPoint)

    pixelsToDraw.foreach({
      case (i, j) => screen(WIDGET_SIZE - j - 1)(i) = handSymbol
    })
  }

  private def drawClockFrame()(implicit screen: Array[Array[Char]]): Unit = {
    screen(WIDGET_SIZE/2)(WIDGET_SIZE/2) = CLOCK_FRAME_CENTER_SYMBOL
    (0 until WIDGET_SIZE).foreach(i => {
      screen(0)(i) = CLOCK_FRAME_HORIZONTAL_SYMBOL
      screen(i)(0) = CLOCK_FRAME_VERTICAL_SYMBOL
      screen(WIDGET_SIZE - 1)(i) = CLOCK_FRAME_HORIZONTAL_SYMBOL
      screen(i)(WIDGET_SIZE - 1) = CLOCK_FRAME_VERTICAL_SYMBOL
    })
    screen(0)(0) = CLOCK_FRAME_CORNER_SYMBOL
    screen(WIDGET_SIZE - 1)(0) = CLOCK_FRAME_CORNER_SYMBOL
    screen(0)(WIDGET_SIZE - 1) = CLOCK_FRAME_CORNER_SYMBOL
    screen(WIDGET_SIZE - 1)(WIDGET_SIZE - 1) = CLOCK_FRAME_CORNER_SYMBOL
  }

  def main(args: Array[String]): Unit = {
    while(true){
      println(getAsciiTimeNow())
      Thread.sleep(1000)
    }
  }
}
