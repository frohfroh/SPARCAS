package analysis

import bcfw.FinishingAnalisator
import bank.Node
import bank.Link
import scala.reflect.ClassTag
import breeze.linalg.{DenseMatrix => DM , _}
import bank.BreezeMatrix

     //TODO check if result is good and the same as MatrixResults

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
case class BreezeMatrixResultParameter[ A, B, R](attributes_filter: Link => A,
                          attributes_value: Link => B,
                          filter: Seq[A] => Boolean, 
                          evaluator: Seq[B] => R, 
                          aggregator:((Double, R),(Double,R)) => (Double, R))   
/**
 * creates analysators to pass to an assignment
 * The analysis allows to determine an aggregation for each OD pair of a characteristic(calculation) of the paths
 * 
 * Of the paths used by each OD pair, one can filter a subset and then calculate a value for each of these paths, and
 * then aggregate these values
 */
object BreezeMatrixResult {
  //the DM[(Double, R)] is a matrix of (Double, R) where
  //the double is the associated demand to this cell(normalized)
  //R is the value obtained by this demand
  type BreezeMatrixResultFA[R] =  FinishingAnalisator[DM[(Double, R)], BreezeMatrix[R ,DM[R] ]]
  type BreezeMatrixResultReturn[R] =     (Map[Node, Int], Map[Link, Int]) => BreezeMatrixResultFA[R]
 
  //TODO rename foo apply
  /**
   * this method is used as a factory for MatrixResult 
	 *@param mrp see the case class MatrixResultParameter for understanding the fields
   * The demand is not the "real" demand, as it is actually the proportion of retained paths. 
   * The calculation of the new demand (aggregator) is somewhat unnecessary and may be removed in the future, as it is not clear if there 
   * is any other way of doing it but demand1 * proportion1 + demand2 * proportion2 
   * It is maybe useful is one wants to get maximum values? Great care must be taken as a path taken at 
   * one iteration may not be present in the optimal. BCFW would take it (slowly) to 0.0 but such a maximum 
   * would not be correct
   */
  def foo[ A, B, R: ClassTag](mrp : BreezeMatrixResultParameter[ A, B, R]): BreezeMatrixResultReturn[R] = {     
      (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) =>
        new BreezeMatrixResult(mrp,NodeToPos, LinkToPos)
            
  }
}
//A attributes for filter
//B attributes for calculation
//R value of each path that is also the final result

 class BreezeMatrixResult[ A, B, R : ClassTag](val mrp : BreezeMatrixResultParameter[ A, B, R] , 
        val NodeToPos: Map[Node, Int], val LinkToPos: Map[Link, Int])
    extends  BreezeMatrixResult.BreezeMatrixResultFA[R] {

  val pos_Link = LinkToPos.map(_.swap).toArray
  val pos2node = NodeToPos.map(_.swap)
  val Links = pos_Link.sortBy(_._1).map(_._2)
  val fAttributes = Links.map({ mrp.attributes_filter(_) })
  val vAttributes = Links.map({ mrp.attributes_value(_) })

  //TODO does using the matriz_od is any better then normalize everything and deal with it later(after assignment, outside of this framework)?
  def simplePath(path_matrix: Array[Array[Array[Int]]], matriz_od: Array[Array[Double]]): DM[(Double, R)] = { 
    
     val dm = bank.BreezeMatrix.fromAoA(path_matrix)
     val filterLinkValue2 = dm.mapValues { _.map { fAttributes } }
     val filteredB2 = filterLinkValue2.mapValues { mrp.filter }
     val filtered2 = filteredB2.mapValues { if (_) 1.0 else 0.0 }
     val valueLinkValue2 = dm.mapValues { _.map { vAttributes } }
     val evaluated2 = valueLinkValue2.mapValues {  mrp.evaluator(_) }
     filtered2 zip evaluated2
  }
 def combine(left: DM[(Double, R)], right: DM[(Double, R)], left_proportion: Double): DM[(Double, R)] = {
    val right_proportion = 1.0 - left_proportion
    def comb(lvr : (Double,R), rvr :(Double,R)) = {
      val (lv,lr) = lvr
      val (rv, rr) = rvr
      mrp.aggregator((lv * left_proportion, lr), (rv * right_proportion, rr))
    }
    DM.zipMap[(Double,R),(Double,R)].map(left,right , comb)
  }

  def finalize(brute: DM[(Double, R)]) :BreezeMatrix[R,DM[R]] = {
    val cleanM = brute.mapValues(_._2)
    val e = new BreezeMatrix[R,DM[R]](cleanM , pos2node)
    e
  }
}
 
 