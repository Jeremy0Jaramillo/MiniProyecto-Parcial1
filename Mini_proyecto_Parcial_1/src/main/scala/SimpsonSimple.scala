def integracion( f:Double=>Double, a:Double, b:Double):Double =
  (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)


val ejem1 = integracion(x => (-math.pow(x,2) + 8*x  - 12), 3, 5)
val ejem2 = integracion(x => 3*(math.pow(x,2)), 0, 2)
val ejem3 = integracion(x => x+(2*(math.pow(x,2)))-(math.pow(x,3))+(5*(math.pow(x,4))),-1,1)
val ejem4 = integracion(x => (2*x+1)/((math.pow(x,2))+x) ,1,2)
val ejem5 = integracion(x => math.pow((math.E),x) ,0, 1)
val ejem6 = integracion(x => 1 / math.sqrt(x-1),2,3)
val ejem7 = integracion(x => 1 / (1 + math.pow(x,2)),0,1)


@main def app2: Unit=
  println(ejem1)
  println(ejem2)
  println(ejem3)
  println(ejem4)
  println(ejem5)
  println(ejem6)
  println(ejem7)