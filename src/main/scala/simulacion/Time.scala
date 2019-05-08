package simulacion

case class Time(daySeconds: Int) {

  def add(seconds: Int): Time = copy(daySeconds = daySeconds + seconds)

  def isPeakTime: Boolean = (daySeconds % 86400 <= 57600) && (daySeconds % 86400 >= 28800)

  def greaterThan(time: Time): Boolean = daySeconds > time.daySeconds

}

case class TimeLapse(start: Int, finish: Int) {

  def dif: Int = finish - start

}
