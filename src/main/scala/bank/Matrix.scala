package bank

abstract class Matrix[T] { //TODO make matriz generic and not Double only
  def toArrays(mapa: Map[Node, Int]): Array[Array[T]]
  def value(par: (Node, Node)): T
}