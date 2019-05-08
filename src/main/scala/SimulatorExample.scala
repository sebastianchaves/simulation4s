import App.IA
import RequestStatus.RequestStatus

import scala.util.Random

object Simulator {

  def run(NSA: Int, CCN: Int, TF: Int): Unit = {

    val finalTime = Time(TF)

    val nodes = for {
      _ <- (1 to NSA).toList
    } yield Node(CCN)

    val requests = {

      def rec(startTime: Time, endTime: Time): List[Request] = {

        val request = Nil

        if(startTime.greaterThan(endTime)) {
          requests
        } else {

          nodes.find(_.idIdle)

          IAT(time)

          rec(startTime, endTime)
        }

      }

      rec(Time(0), finalTime)
    }

    val processTimeAvg = requests.map(_.processedTime).sum / requests.length
    val idleTimeAvg = nodes.map(_.idleTime).sum / finalTime.daySeconds
    val errorPercentaje = requests.count(_.status == RequestStatus.Error) * 100.0 / requests.length
    val canceledPercentaje = requests.count(_.status == RequestStatus.Canceled) * 100.0 / requests.length

    print(processTimeAvg)
    print(idleTimeAvg)
    print(errorPercentaje)
    print(canceledPercentaje)
  }

  def IAT(actualTime: Time): Float = if (actualTime.isPeakTime) IA else IA * 15

  def IT: Float = {
    val M = 0.08d
    var xi = 0d
    var yi = 1d
    var fx = 0d
    while (fx < yi) {
      xi = 4 + Random.nextDouble * 120
      yi = Random.nextDouble * M
      fx = Math.exp(-0.5 * Math.pow((xi - 15) / 5, 2.0)) / 37.5994241195
    }
    xi.toFloat
  }

}

case class Node(cores: Int, idleTimes: List[TimeLapse] = Nil) {

  def idleTime: Int = idleTimes.map(_.dif).sum

  def addIdleTime(idle: TimeLapse): Node = copy(idleTimes = idleTimes :+ idle)

  def status =

}

case class Request(status: RequestStatus = RequestStatus.Ok, start: Int, finish: Int) {

  def canceled: Request = copy(status = RequestStatus.Canceled)

  def processedTime: Int = finish - start

}

object Request {
  def apply(status: RequestStatus): Request = {
    if (Random.nextDouble() <= 0.05) Request(RequestStatus.Canceled) else Request(RequestStatus.Ok)
  }
}

object RequestStatus extends Enumeration {
  type RequestStatus = Value
  val Ok, Error, Canceled = Value
}

case class Time(daySeconds: Int) {

  def add(seconds: Int): Time = copy(daySeconds = daySeconds + seconds)

  def isPeakTime: Boolean = (daySeconds % 86400 <= 57600) && (daySeconds % 86400 >= 28800)

  def greaterThan(time: Time): Boolean = daySeconds > time.daySeconds

}

case class TimeLapse(start: Int, finish: Int) {

  def dif: Int = finish - start

}
