def Compuesta(a : Int, b: Int, n: Int ,f :Double=> Double):Double ={
  val h = ( b - a )/n.toDouble
  val xj = (j : Double) => (a + j*h)
  val func = (j : Double) => f(xj(2 * j - 2)) + (4 * f(xj(2 * j - 1))) + (f(xj(2 * j)))
  ((1 to n/2).map(func(_)).sum) * h/3
}

val ec1 = (x : Double) => (-math.pow(x,2) + 8*x  - 12)
val compuesta1 = Compuesta(3, 5, 70, ec1)
val ec2 = (x : Double) => 3*(math.pow(x,2))
val compuesta2 = Compuesta(0 , 2 , 72 ,ec2)
val ec3 = (x : Double) => x+(2*(math.pow(x,2)))-(math.pow(x,3))+(5*(math.pow(x,4)))
val compuesta3 = Compuesta(-1 , 1 , 74 ,ec3)
val ec4 = (x : Double) => (2*x+1)/((math.pow(x,2))+x)
val compuesta4 = Compuesta(1 , 2 , 76 ,ec4)
val ec5 = (x : Double) => math.pow((math.E),x)
val compuesta5 = Compuesta(0 , 1 , 78 ,ec5)
val ec6 = (x : Double) => 1 / math.sqrt(x-1)
val compuesta6 = Compuesta(2 , 3 , 80 ,ec6)
val ec7 = (x : Double) => 1 / (1 + math.pow(x,2))
val compuesta7 = Compuesta(0 , 1 , 82 ,ec7)


@main def proyecto: Unit =
  println(compuesta1)
  println(compuesta2)
  println(compuesta3)
  println(compuesta4)
  println(compuesta5)
  println(compuesta6)
  println(compuesta7)