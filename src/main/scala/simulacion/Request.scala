package simulacion

import RequestStatus.RequestStatus

import scala.util.Random

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