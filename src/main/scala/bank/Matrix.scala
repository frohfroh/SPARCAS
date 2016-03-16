package bank

abstract class Matrix {
  def toArrays(mapa: Map[Node, Int]): Array[Array[Double]]
  def value(par: (Node, Node)): Double
}