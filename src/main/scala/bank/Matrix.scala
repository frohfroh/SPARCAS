package bank

abstract class Matrix { //TODO make matriz generic and not Double only
  def toArrays(mapa: Map[Node, Int]): Array[Array[Double]]
  def value(par: (Node, Node)): Double
}