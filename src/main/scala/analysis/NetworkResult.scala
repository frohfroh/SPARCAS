package analysis

import bcfw.FinishingAnalisator
import bank.Node
import bank.Link
import scala.reflect.runtime.{ universe => ru }
import bank.Matrix
/**
 * This object creates analysators to pass to an assignment
 * The analysis allows to determine the flow on each link of paths corresponding to a criteria.
 * 
 */
object NetworkResult {
  /**creates an analysator
   * 
   * @param  attributes this function shall get all information one wants from a link and return an object with these values.
 * While it is expected that most often it will simply get network attributes from links(hence its name), it can do anything based 
 * on a link object and not only its attributes
 * @param filter Given a sequence of attributes (as prepared by the parameter attribute) , decide if it includes or not a path on the analysis
 * @param slaveDemand while the assignment will always be performed according to the demand passed to the assign method, the flows
 * resulting from this analysis can be of another matrix. If None , the own demand matrix will be used
 * 
   */
    @deprecated("use teh method create with a matrix and not an array","SNAPSHOT") 
  def apply[A](attributes: Link => A,
      filter: Seq[A] => Boolean,
      slaveDemand: Option[Array[Array[Double]]] = None): (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[Array[Double], Map[Link, Double]] = {
    (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) => NetworkResult[A](attributes, filter, slaveDemand, NodeToPos, LinkToPos)
  }
      
                   //TODO remove method above and rename create to apply
                 /**creates an analysator
   * 
   * @param  attributes this function shall get all information one wants from a link and return an object with these values.
 * While it is expected that most often it will simply get network attributes from links(hence its name), it can do anything based 
 * on a link object and not only its attributes
 * @param filter Given a sequence of attributes (as prepared by the parameter attribute) , decide if it includes or not a path on the analysis
 * @param slaveDemand while the assignment will always be performed according to the demand passed to the assign method, the flows
 * resulting from this analysis can be of another matrix. If None , the own demand matrix will be used
 * 
   */
  def create[A](attributes: Link => A, 
               filter: Seq[A] => Boolean, 
               slaveDemand: Option[Matrix[Double]] = None): (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[Array[Double], Map[Link, Double]] = {
    (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) => {
      val nm = slaveDemand.map { _.toArrays(NodeToPos) }
      NetworkResult[A](attributes, filter, nm, NodeToPos, LinkToPos)
    }
  }
}

case class NetworkResult[A](val attributes: Link => A, val filter: Seq[A] => Boolean,
    val slaveDemand: Option[Array[Array[Double]]], val NodeToPos: Map[Node, Int], val LinkToPos: Map[Link, Int]) extends FinishingAnalisator[Array[Double], Map[Link, Double]] {

  val links = LinkToPos.size
  val pos_Link = LinkToPos.map(_.swap).toArray
  val Links = pos_Link.sortBy(_._1).map(_._2)
  val gimmeAttributes = Links.map({ attributes(_) })

  def simplePath(path_matrix: Array[Array[Array[Int]]], matriz_od: Array[Array[Double]]): Array[Double] = {
    val atts = path_matrix.map { linha => linha.map { celula => celula.map(el => gimmeAttributes(el)) } }
    val taken = atts.map { linha => linha.map { celula => filter(celula) } }
    val matrix = slaveDemand.getOrElse(matriz_od)
    val vols = new Array[Double](links) //colocar os nós
    val n = path_matrix.length
    for (from <- 0 until n) {
      for (to <- 0 until n) {
        if (taken(from)(to)) {
          for (l <- path_matrix(from)(to)) {
            vols(l) += matrix(from)(to)
          }
        }
      }
    }
    vols
  }

  def combine(left: Array[Double], right: Array[Double], left_proportion: Double): Array[Double] = {
    val righ_proportion = 1 - left_proportion
    val vols = new Array[Double](links)
    for (i <- 0 until vols.size) {
      vols(i) = left(i) * left_proportion + right(i) * righ_proportion
    }
    vols
  }

  def finalize(brute: Array[Double]): Map[Link, Double] = {
    val PosToLink = LinkToPos.map({ case (a, b) => (b, a) }).toMap
    (0 until brute.size).map(v => (PosToLink(v), brute(v))).toMap
  }

}