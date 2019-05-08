package simulacion

case class Node(cores: Int, idleTimes: List[TimeLapse] = Nil) {

  def idleTime: Int = idleTimes.map(_.dif).sum

  def addIdleTime(idle: TimeLapse): Node = copy(idleTimes = idleTimes :+ idle)

  def status =

}

object NodeStatus extends Enumeration {
  type RequestStatus = Value
  val Ok, Error, Canceled = Value
}
