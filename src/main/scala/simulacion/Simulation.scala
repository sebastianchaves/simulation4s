package simulacion

import scala.util.Random

object Simulation {

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

}

object AuxiliarRoutines {

  def IAT(actualTime: Time): Float = if (actualTime.isPeakTime) IA else IA * 15

  def IA :Float = {
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

  def MIN(array: Array[Float]): Int = array.indexOf(array.min)

  def TRBD: Float = (15f + Math.sqrt(225f - 150f * (Random.nextDouble + 4f / 3f))).toFloat

  def TRR: Float = Math.sqrt(1500f * Random.nextDouble - 100f).toFloat

}
