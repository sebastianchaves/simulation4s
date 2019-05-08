import scala.util.Random

object Simulation {

  def run(NSF: Integer, NCF: Int, NSB: Int, NCB: Int, TF: Int) = {

    var T = 0f
    var TPReq = 0f
    var SPS = 0f
    var CR = 0
    var NRE = 0
    var NRR = 0
    var TPRes = Array.fill(NSF*NCF) { Float.MaxValue }
    var NS = Array.fill(NSF) { 0f }
    var TC = Array.fill(NSB*NCB) { 0f }
    var TBF = 0f
    var i = 0

    def BACKEND = {
      val TB = if(Random.nextDouble <= 0.2) TBD else TCP
      val minTC = MEN(TC)
      TC(minTC) = if (T <= TC(minTC)) TC(minTC) + TB else T + TB
      TBF = TC(minTC)
    }

    def FRONTEND = {
      BACKEND
      TPRes(i) = TBF + TR
      if (Random.nextDouble <= 0.05 || TPRes(i) - T > 5) NRE = NRE + 1
    }

    def IRT = if (T % 86400 <= 36000) IR * 10 else IR

    while (T <= TF) {
      print(s"Tiempo de Corrida: ${T.toInt}/$TF(${(T/TF*100).toInt}%)\r")
      i = MEN(TPRes)
      if (TPReq <= TPRes(i)) {
        NS.foreach { ns => SPS = SPS + (TPReq - T) * ns}
        T = TPReq
        TPReq = T + IRT
        i = MEN(NS)
        if(NS(i) < 75 + NCF) {
          NS(i) = NS(i) + 1
          CR = CR + 1
          if (NS(i) <= NCF) {
            i = i*NCF + TPRes.slice(i*NCF, (i+1)*NCF).indexOf(Float.MaxValue)
            FRONTEND
          }
        }
        else {
          NRR = NRR + 1
        }
      }
      else {
        NS.foreach {ns => SPS = SPS + (TPRes(i) - T) * ns}
        T = TPRes(i)
        NS(i / NCF) = NS(i / NCF) - 1
        if (NS(i / NCF) >= NCF) FRONTEND else TPRes(i) = Float.MaxValue
      }
    }

    val PPS = SPS.toDouble / CR
    val PRR = NRR * 100.0 / CR
    val PRE = NRE * 100.0 / CR

    println("Fin de Corrida")
    println(s"   Tiempo: $T")
    println(s"   Total de Requests Procesadas: $CR")
    println(f"   Promedio de Tiempo de Respuesta: $PPS%2.2f")
    println(f"   Porcentaje de Request Rechazadas: $PRR%2.2f%% ($NRR)")
    println(f"   Porcentaje de Request Erroneas: $PRE%2.2f%% ($NRE)")
  }

  def MEN(array: Array[Float]) = array.indexOf(array.min)

  def IR = {
    val M = 16d
    var xi = 0d
    var yi = 1d
    var fx = 0d
    while (fx < yi) {
      xi = 0.0027 + Random.nextDouble * 0.2223
      yi = Random.nextDouble * M
      fx = Math.exp(-0.5 * Math.pow((xi - 0.1) / 0.025, 2.0)) / 0.0626657
    }
    xi.toFloat
  }

  def TBD = (-Math.sqrt((9f/16f) - (147f/400f) * (Random.nextDouble + (29f/147f))) + 3f/4f).toFloat

  def TCP = (Math.sqrt(15 * Random.nextDouble + 1)).toFloat

  def TR = (0.2 * Random.nextDouble + 0.3).toFloat

}

object App {

  def simulate(NSA: Int, CCN: Int, TF: Int): Unit = {

    var T = 0f
    var TPP = 0f
    var STP = 0f
    var STR = 0f
    var STO = 0f
    var NR = 0
    var NRE = 0
    var NRC = 0
    var ITO = 0f
    var TPR = Array.fill(NSA * CCN) { Float.MaxValue }
    var NS = Array.fill(NSA) { 0f }
    var i = 0

    def PROCESAR = {
      val TP = if(Random.nextDouble <= 0.2) TRR else TRBD
      TPR(MIN(TPR)) = TP
      if(Random.nextDouble < 0.08) NRE = NRE + 1
    }

    def IAT = if ((T % 86400 <= 57600) && (T % 86400 >= 28800)) IA else IA * 15

    while (T <= TF) {
      print(s"Tiempo de Corrida: ${T.toInt}/$TF(${(T/TF*100).toInt}%)\r")
      i = MIN(TPR)
      if (TPP <= TPR(i)) {
        T = TPP
        STP = STP + TPP
        TPP = T + IAT
        i = MIN(NS)
        if(NS(i) <= 110) {
          NS(i) = NS(i) + 1
          NR = NR + 1
          if (NS(i) == 1) {
            STO = STO + (T - ITO)
            PROCESAR
          }
        } else {
          NRC = NRC + 1
        }
      } else { // RESPUESTa
        T = TPR(i)
        STR = STR + TPR(i)
        NS(i) = NS(i) - 1
        if (NS(i) >= 1) PROCESAR
        else {
          ITO = T
          TPR(i) = Float.MaxValue
        }
      }
    }

    val PTP = (STR - STP) / NR
    val PTO = STO / T
    val PPE = NRE * 100.0 / NR
    val PPC = NRC * 100.0 / NR

    println("Fin de Corrida")
    println(s"   Tiempo: $T")
    println(s"   Total de Requests Procesadas: $NR")
    println(f"   Promedio de Tiempo de Procesamientp: $PTP%2.2f%% ($NR)")
    println(f"   Porcentaje de Tiempo Ocioso: $PTO%2.2f%% ($T)")
    println(f"   Porcentaje de Pedidos Erroneas: $PPE%2.2f%% ($NRE)")
    println(f"   Porcentaje de Pedidos Cancelados: $PPC%2.2f%% ($NRC)")
  }

  def MIN(array: Array[Float]) = array.indexOf(array.min)

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

  def TRBD = (15f + Math.sqrt(225f - 150f * (Random.nextDouble + 4f / 3f))).toFloat

  def TRR = Math.sqrt(1500f * Random.nextDouble - 100f).toFloat

}
