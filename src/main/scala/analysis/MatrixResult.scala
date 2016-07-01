package analysis

import bcfw.FinishingAnalisator
import bank.Node
import bank.Link


/**
 * only used to organize the parameter of MatrixResult apply and avoiding copying this huge list and signature multiple times
   * @param attributes_filter  this function shall get all information one wants from a links and return an object with these values.
   * While it is expected that most often it will simply get network attributes from links(hence its name), it can do anything based 
   * on a link object and not only its attributes. This refers only to the information needed for the filter
   * @param attributes_value same as above but for the values of the paths. This refers only to the information needed for calculating the value of each path
   * @param filter Given a sequence of attributes (as prepared by the parameter attributes_filter) , decide if it includes or not a link on the analysis
   * @param evaluator Given a sequence of values of links in a path (as prepared by the parameter attributes_value) , calculates the value of the whole path
   * @param aggregator Given two paths with their values and respective demands, calculates a new value and combined demand.
 */
  case class MatrixResultParameter[ A, B, R](attributes_filter: Link => A,
                          attributes_value: Link => B,
                          filter: Seq[A] => Boolean, 
                          evaluator: Seq[B] => R, 
                          aggregator:((Double, R),(Double,R)) => (Double, R))   
/**
 * creates analysators to pass to an assignment
 * The analysis allows to determine an aggregation for each OD pair of a characteristic of the paths
 * 
 * Of the paths used by each OD pair, one can filter a subset and then calculates a value for each of these paths, and
 * then aggregate these values
 */
object MatrixResult {
  //the Array[Array[(Double, R)] is a matrisx of (Double, R) where
  //the double is the assiciated demand to this cell(normalized)
  //R is the value obtained by this demand
  type MatrixResultFA[R] =  FinishingAnalisator[Array[Array[(Double, R)]], scala.collection.Map[Node, Map[Node, R]]]
type MatrixResultReturn[R] = 
    (Map[Node, Int], Map[Link, Int]) => MatrixResultFA[R]
 

  /**
	 *@param mrp see the case class MatrixResultParameter for understanding the fields
   * The demand is not the "real" demand, as it is actually the proportion of retained paths. 
   * The calculation of the new demand (aggregator) is somewhat unnecessary and may be removed in the future, as it is not clear if there 
   * is any other way of doing it but demand1 * proportion1 + demand2 * proportion2 
   * It is maybe useful is one wants to get maximum values? Great care must be taken as a path taken at 
   * one iteration may not be present in the optimal. BCFW would take it (slowly) to 0.0 but such a maximum 
   * would not be correct
   */
  def foo[ A, B, R](mrp : MatrixResultParameter[ A, B, R]): MatrixResultReturn[R] = {     
      (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) =>
        new MatrixResult(mrp,NodeToPos, LinkToPos)
            
  }
      //TODO substitute apply by foo
      @deprecated( "use foo", "SNAPSHOT" )
      def apply[ A, B, R](attributes_filter: Link => A,
                          attributes_value: Link => B,
                          filter: Seq[A] => Boolean, 
                          evaluator: Seq[B] => R, 
                          aggregator:((Double, R),(Double,R)) => (Double, R)): 
      (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[Array[Array[(Double, R)]], scala.collection.Map[Node, Map[Node, R]]] = {
         
      (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) =>
        new MatrixResult(MatrixResultParameter[  A, B, R](attributes_filter, attributes_value, filter, evaluator, aggregator),NodeToPos, 
            LinkToPos)
            
  }
}
//A attributes for filter
//B attributes for calculation
//R value of each path that is also the final result

//TODO once matrix(or arrray matrix) is generic , get rid of the Map[Node,Map[Node,?]] , see multi dimensional arrays
//with that une can reimplement map  

 class MatrixResult[ A, B, R](val mrp : MatrixResultParameter[ A, B, R] , 
        val NodeToPos: Map[Node, Int], val LinkToPos: Map[Link, Int])
    extends  MatrixResult.MatrixResultFA[R] {

  val pos_Link = LinkToPos.map(_.swap).toArray
  val pos2node = NodeToPos.map(_.swap)
  val Links = pos_Link.sortBy(_._1).map(_._2)
  val fAttributes = Links.map({ mrp.attributes_filter(_) })
  val vAttributes = Links.map({ mrp.attributes_value(_) })

  //TODO does using the matriz_od is any better then normalize everything and deal with it later(after assignment, outside of this framework)?
  def simplePath(path_matrix: Array[Array[Array[Int]]], matriz_od: Array[Array[Double]]): Array[Array[(Double, R)]] = {    
    val filterLinkValue = path_matrix.map { linha => linha.map { celula => celula.map(l => fAttributes(l)) } }
    val filteredB = filterLinkValue.map { linha => linha.map { celula => mrp.filter(celula) } } 
    val filtered = filteredB.map { linha => linha.map { celula => if (celula) 1.0 else 0.0 } }
    val valueLinkValue = path_matrix.map { linha => linha.map { celula => celula.map(l => vAttributes(l)) } }
    val evaluated = valueLinkValue.map { linha => linha.map { celula => mrp.evaluator(celula) } }
    val zipLines = filtered zip evaluated
    zipLines.map({ case (bools, rs) => bools zip rs })
  }

  def combine(left: Array[Array[(Double, R)]], right: Array[Array[(Double, R)]], left_proportion: Double): Array[Array[(Double, R)]] = {
    val n = left.length
    val right_proportion = 1.0 - left_proportion
    val ans = new Array[Array[(Double, R)]](n)
    for (i <- 0 until n) {
      val z = left(i) zip right(i)
      ans(i) = z.map({ case ((lv, lr), (rv, rr)) => mrp.aggregator((lv * left_proportion, lr), (rv * right_proportion, rr)) })
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
 
 