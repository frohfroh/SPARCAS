package analysis

import bcfw.FinishingAnalisator
import bank.Node
import bank.Link
/**
 * This object creates analysators to pass to an assignment
 * The analysis allows to determine an aggregation for each OD pair of a characteristic of the paths
 * 
 * Of the paths used by each OD pair, one can filter a subset and then calculates a value for each of these paths, and
 * then aggregating these values
 */
object MatrixResult {
  /**
   * @param attributes_filter  this function shall get all information one wants from a links and return an object with these values.
   * While it is expected that most often it will simply get network attributes from links(hence its name), it can do anything based 
   * on a link object and not only its attributes. This refers only to the information needed for the filter
   * @param attributes_value same as above but for the values of the paths. This refers only to the information needed for calculating the value of each path
   * @param filter Given a sequence of attributes (as prepared by the parameter attributes_filter) , decide if it includes or not a link on the analysis
   * @param evaluator Given a sequence of values of links in a path (as prepared by the parameter attributes_value) , calculates the value of the whole path
   * @param aggregator Given two paths with their values and respective demands, calculates a new value and combined demand.
   * The demand is not the "real" demand, as it is actually the proportion of retained paths. 
   * The calculation of the new demand is somewhat unnecessary and may be removed in the future, as it is not clear if there 
   * is any other way of doing it but demand1 * proportion1 + demand2 * proportion2 
   * 
   */
  def apply[T, F, A, B, R](attributes_filter: Link => A, attributes_value: Link => B,
                          filter: Seq[A] => Boolean, evaluator: Seq[B] => R, 
                          aggregator:((Double, R),(Double,R)) => (Double, R)): 
      (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[Array[Array[(Double, R)]], scala.collection.Map[Node, Map[Node, R]]] = {
         
      (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) =>
        MatrixResult(attributes_filter, attributes_value, filter, evaluator, aggregator,NodeToPos, 
            LinkToPos)
            
  }
}
//T intermediqte
//A attributes for filter
//B attributes for calculation
//R value of each path that is also the final result

//T é uma matriz de valores e médias
//TODO once matrix(or arrray matrix) is generic , get rid of the Map[Node,Map[Node,?]] , see multi dimensional arrays
//with that une can reimplement map  
case class MatrixResult[T, F, A, B, R](val attributes_filter: Link => A, val attributes_value: Link => B,
  val filter: Seq[A] => Boolean, val evaluator: Seq[B] => R, val aggregator: ((Double, R),(Double,R)) => (Double, R),
  val NodeToPos: Map[Node, Int], val LinkToPos: Map[Link, Int])
    extends FinishingAnalisator[Array[Array[(Double, R)]], scala.collection.Map[Node, Map[Node, R]]] {

  val pos_Link = LinkToPos.map(_.swap).toArray
  val pos2node = NodeToPos.map(_.swap)
  val Links = pos_Link.sortBy(_._1).map(_._2)
  val fAttributes = Links.map({ attributes_filter(_) })
  val vAttributes = Links.map({ attributes_value(_) })

  //TODO does using the matriz_od is any better then normalize everything and deal with it later(after assignment, outside of this framework)
  //get rid of slaveDemand
  def simplePath(path_matrix: Array[Array[Array[Int]]], matriz_od: Array[Array[Double]]): Array[Array[(Double, R)]] = {
    val filterLinkValue = path_matrix.map { linha => linha.map { celula => celula.map(l => fAttributes(l)) } }
    val filteredB = filterLinkValue.map { linha => linha.map { celula => filter(celula) } } 
    val filtered = filteredB.map { linha => linha.map { celula => if (celula) 1.0 else 0.0 } }
    val valueLinkValue = path_matrix.map { linha => linha.map { celula => celula.map(l => vAttributes(l)) } }
    val evaluated = valueLinkValue.map { linha => linha.map { celula => evaluator(celula) } }
    val zipLines = filtered zip evaluated
    zipLines.map({ case (bools, rs) => bools zip rs })
  }

  def combine(left: Array[Array[(Double, R)]], right: Array[Array[(Double, R)]], left_proportion: Double): Array[Array[(Double, R)]] = {
    val n = left.length
    val right_proportion = 1.0 - left_proportion
    val ans = new Array[Array[(Double, R)]](n)
    for (i <- 0 until n) {
      val z = right(i) zip left(i)
      ans(i) = z.map({ case ((lv, lr), (rv, rr)) => aggregator((lv * left_proportion, lr), (rv * right_proportion, rr)) })
    }
    ans
  }

  def finalize(brute: Array[Array[(Double, R)]]): scala.collection.Map[Node, Map[Node, R]] = {
    val n = brute.length
    val ans = scala.collection.mutable.Map[Node, Map[Node, R]]()

    for (i <- 0 until n) yield {
      val node_from = pos2node(i)
      val smap = ((0 until n) zip brute(i)).map({ case (t, (d, r)) => (pos2node(t), r) }).toMap
      ans(node_from) = smap
    }
    ans
  }
}