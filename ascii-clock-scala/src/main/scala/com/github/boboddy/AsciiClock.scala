package com.github.boboddy

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.Date
import java.util.concurrent.TimeUnit

object AsciiClock {
  import LineAlgo.Point
  val CLOCK_SIZE = 31

  def getAsciiTimeNow: String = {
    val Array(hours, minutes, seconds) = new SimpleDateFormat("HH:mm:ss").format(Date.from(Instant.now))
      .split(":").map(_.toInt)

    val hoursAngleDegrees: Double = getAngleOf(hours, TimeUnit.HOURS)
    val minutesAngleDegrees: Double = getAngleOf(minutes, TimeUnit.MINUTES)
    val secondsAngleDegrees: Double = getAngleOf(seconds, TimeUnit.SECONDS)

    val hoursHandPoints: Seq[Point] = getClockHandPoints(hoursAngleDegrees, handSizeProportion = 0.5)
    val minutesHandPoints: Seq[Point] = getClockHandPoints(minutesAngleDegrees)
    val secondsHandPoints: Seq[Point] = getClockHandPoints(secondsAngleDegrees)

    val renderedClock: String = Rendering.drawClock(
      hoursHandPoints = hoursHandPoints,
      minutesHandPoints = minutesHandPoints,
      secondsHandPoints = secondsHandPoints
    )

    renderedClock
  }

  private def getAngleOf(timeQuant: Int, timeUnit: TimeUnit): Double = {
    val fraction: Double = timeUnit match {
      case TimeUnit.HOURS => (timeQuant % 12).toDouble / 12D
      case TimeUnit.MINUTES | TimeUnit.SECONDS => timeQuant.toDouble / 60D
      case _ => throw new Exception(s"Unsupported time unit ${timeUnit}")
    }

    fraction * 360D
  }

  private def getClockHandPoints(angleOfHandDeg: Double, handSizeProportion: Double = 1D): Seq[Point] = {
    val halfClockSize: Int = CLOCK_SIZE / 2
    val handSize: Double = halfClockSize * handSizeProportion

    //cartesian axis are not the same as clock axis. Angle starts at 3 o'clock (90 degrees shift) and measured counter clock-wise
    val angleOfHandDegCartesian: Double = (90 + (360 - angleOfHandDeg)) % 360
    val angleOfHandRadCartesian: Double = math.toRadians(angleOfHandDegCartesian)
    val handProjX: Int = (handSize * math.cos(angleOfHandRadCartesian)).toInt
    val handProjY: Int = (handSize * math.sin(angleOfHandRadCartesian)).toInt

    val handStartPoint: Point = (halfClockSize, halfClockSize)
    val handEndPoint: Point = (halfClockSize + handProjX, halfClockSize + handProjY)

    LineAlgo.getPixelsOfLineBresenham(handStartPoint, handEndPoint)
  }



  def main(args: Array[String]): Unit = {
    while(true){
      println(getAsciiTimeNow)
      Thread.sleep(1000)
    }
  }
}
