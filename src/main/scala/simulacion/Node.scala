package simulacion

import scala.util.Random

case class Node(cores: Int, idleTimes: List[TimeLapse] = Nil, busyTimes: List[TimeLapse] = Nil) {

  def idleTime: Int = idleTimes.map(_.dif).sum

  def addIdleTime(idle: TimeLapse): Node = copy(idleTimes = idleTimes :+ idle)

  def addBusyTime(busy: TimeLapse): Node = copy(busyTimes = busyTimes :+ busy)

  def isIdleAt(time: Time): Boolean = busyTimes.exists(_.finish < time.daySeconds)

  def isBusyAt(time: Time): Boolean = busyTimes.exists(_.finish >= time.daySeconds)

  def process(time: Time): Request =
    Request(RequestStatus.Ok,
            TimeLapse(time.daySeconds,
                      if(Random.nextDouble <= 0.2) AuxiliarRoutines.TRR.toInt else AuxiliarRoutines.TRBD.toInt))

}

object NodeStatus extends Enumeration {
  type RequestStatus = Value
  val Ok, Error, Canceled = Value
}
