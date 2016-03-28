package analysis
import bank.Link
import bank.Node
/**
 * keeps the link volumes of an assignment
 */
//TODO retirar Factory e transformar simplesmente num objeto companheiro?
object LinkVolumesFactory extends SimpleTranslatorFactory[Map[Link, Double], LinkVolumes] {
  def createAnalyser(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]): LinkVolumes = {
    new LinkVolumes(NodeToPos, LinkToPos)
  }
}
//If a link does not appear on the assignment it will not appear on the answer
case class LinkVolumes(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) extends Translator[Map[Link, Double]](NodeToPos, LinkToPos) {
  def combine(left: Map[Link, Double], right: Map[Link, Double], left_proportion: Double): Map[Link, Double] = {
    val links = right.keys ++ left.keys
    val ab = links map (link => (link, right.getOrElse(link, 0.0) * (1 - left_proportion) + left.getOrElse(link, 0.0) * left_proportion))
    val soma = links map (link => (link, right.getOrElse(link, 0.0) * (1 - left_proportion) + left.getOrElse(link, 0.0) * left_proportion))
    val soma2 = soma.toList;
    soma2 toMap
  }

  def simplePath2(caminhos: Map[(Node, Node), Array[Link]], mat: bank.Matrix): Map[Link, Double] = {
    val volumados = (caminhos.toArray map { case (par, links) => (mat.value(par), links) })
    val linksVolumadosRepetidos: Array[(Link, Double)] = volumados flatMap { case (vol, links) => links map ((_, vol)) }
    val linkVolumados: Map[Link, Array[(Link, Double)]] = (linksVolumadosRepetidos groupBy (_._1)) toMap
    val quaseans: Map[Link, Array[Double]] = linkVolumados mapValues (_.map(_._2))
    quaseans mapValues (_.reduce(_ + _))
  }

}