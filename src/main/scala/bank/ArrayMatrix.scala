package bank

class ArrayMatrix(val raw: Array[Array[Double]], val mapa: scala.collection.Map[Int, Node]) extends Matrix {
  val Node2Int = mapa map (_.swap)
  def toArrays(mapaNovo: Map[Node, Int]): Array[Array[Double]] = {
    val oldmap = Node2Int
    val newmap = mapaNovo map (_.swap)
    def translate(i: Int): Int = oldmap(newmap(i))
    val origins = (0 until raw.length).toArray
    val matrix = origins map (i => ((0 until raw.length) map ((i, _))).toArray);
    val translated = matrix map (_ map { case (i, j) => (translate(i), translate(j)) })
    translated map (_ map { case (i, j) => raw(i)(j) })
  }
  def value(par: (bank.Node, bank.Node)): Double = raw(Node2Int(par._1))(Node2Int(par._2))
}