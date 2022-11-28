def Extendida(a : Int, b: Int,f :Double=> Double ) : Double = {

  val n = 2 * (b -a)
  val h = ( b - a )/n.toDouble
  val x = ( n : Double) => f(a + n*h)

  val funciones = f(a) + 4 *(1 to n-1 by 2).map(x(_)).sum + 2 *(2 to n-2 by 2).map(x(_)).sum +f(b)
  h/3 * funciones
}

val ecu1 = ((x : Double) =>  (-math.pow(x,2) + 8*x  - 12))
val extendida1 = Extendida(3, 5, ecu1)
val ecu2 = ((x : Double) => 3*(math.pow(x,2)))
val extendida2 = Extendida(0 , 2  ,ecu2)
val ecu3 = (x : Double) => x+(2*(math.pow(x,2)))-(math.pow(x,3))+(5*(math.pow(x,4)))
val extendida3 = Extendida(-1 , 1  ,ecu3)
val ecu4 = (x : Double) => (2*x+1)/((math.pow(x,2))+x)
val extendida4 = Extendida(1 , 2  ,ecu4)
val ecu5 = (x : Double) => math.pow((math.E),x)
val extendida5 = Extendida(0 , 1  ,ecu5)
val ecu6 = (x : Double) => 1 / math.sqrt(x-1)
val extendida6 = Extendida(2 , 3  ,ecu6)
val ecu7 = (x : Double) => 1 / (1 + math.pow(x,2))
val extendida7 = Extendida(0 , 1 ,ecu7)

@main def proyectoUno: Unit =
  println(extendida1)
  println(extendida2)
  println(extendida3)
  println(extendida4)
  println(extendida5)
  println(extendida6)
  println(extendida7)
