package analysis.commonAnalysis

import bank.Link
import bcfw.FinishingAnalisator
import bank.Node
import analysis.MatrixResult
import analysis.MatrixResultParameter

/**
 * Is used to create select links that should return a matrix 
 */
object LinkSelectorMatrix {
  
  def crossing_links(links : Set[Link])(link: Link): Boolean = { links.contains(link) }
  
  def filter(s: Seq[Boolean]): Boolean = s.exists(x => x)
  
  def get_nothing(link: Link): Unit = {()}
  
  def evaluator(algo: Seq[Boolean]) : Double  = {
    if(algo.exists(x => x)) 1.0
    else 0.0
  }
  
  def filtro(s: Seq[Unit]): Boolean = true

  def agregator(path1: (Double, Double) ,path2: (Double, Double) ): (Double, Double) = {
    (path1._1+path2._1, path1._1*path1._2+path2._1*path2._2)
  }
  
  def MRparameter(links : Set[Link]) : MatrixResultParameter[ Unit, Boolean, Double] = {
    MatrixResultParameter(get_nothing _ ,
                attributes_value = crossing_links(links) _ ,
                filter= filtro _ , evaluator = evaluator _ , aggregator = agregator _)
  }
  /**
   * creates an analysator that gets the matrix of the volumes proportions that pass through certain links
   * If it is multiplied by the demand we get the actual volumes passing through these links
   * one can also get the demand by using demand
   * @param links the set of links
   */
  def apply(links : Set[Link]) :  
    (Map[Node, Int], Map[Link, Int]) => MatrixResult.MatrixResultFA[Double] = {
    MatrixResult.foo(MRparameter(links))
  }
  def apply(links : => Set[Long]) :  
    (Map[Node, Int], Map[Link, Int]) => MatrixResult.MatrixResultFA[Double] = {
     usingLong(MatrixResult.foo _ )(links)
  }
   def demand(links : Set[Link]) :  
    (Map[Node, Int], Map[Link, Int]) => MatrixResult.MatrixResultFA[Double] = {
    VolumeMatrix(MRparameter(links))
  }
  def demand(links : => Set[Long]) :  
    (Map[Node, Int], Map[Link, Int]) => MatrixResult.MatrixResultFA[Double] = {
     usingLong(VolumeMatrix.apply _ )(links)
  }  
  def usingLong(foo :  MatrixResultParameter[Unit,Boolean,Double] => MatrixResult.MatrixResultReturn[Double])(links : => Set[Long]) :  
    (Map[Node, Int], Map[Link, Int]) => MatrixResult.MatrixResultFA[Double] = {
     def init(nodesMap : Map[bank.Node,Int],linksMap : Map[bank.Link,Int]) :  MatrixResult.MatrixResultFA[Double]  = {
      val allLinks = linksMap.keySet
      val id2link = allLinks.map { link => (link.id.id , link) }.toMap
      val selectedLinks = links.map { id2link(_) }
      val fun = foo(MRparameter(selectedLinks))
      fun(nodesMap , linksMap)
     }
     init
  }
    
    
  class VolumeMatrix[ A, B](mrp : MatrixResultParameter[  A, B, Double] ,
   NodeToPos: Map[Node, Int],  LinkToPos: Map[Link, Int])   extends MatrixResult(mrp , NodeToPos , LinkToPos ){
    var mod : Option[Array[Array[Double]]] = None
    override def simplePath(path_matrix: Array[Array[Array[Int]]], matriz_od: Array[Array[Double]]): Array[Array[(Double, Double)]] = {
      mod = Some(matriz_od)
      super.simplePath(path_matrix, matriz_od)
    }
    override def finalize(brute: Array[Array[(Double, Double)]]): scala.collection.Map[Node, Map[Node, Double]] =  {
      val semians = super.finalize(brute)
      val ans = scala.collection.mutable.Map[Node, Map[Node, Double]]()
      val nmod = mod.get
      for((key,im) <- semians){
        val t = scala.collection.mutable.Map[Node, Double]()
        for( (k2 , v) <- im){
          t(k2) = v * nmod(NodeToPos(key))(NodeToPos(k2))
        }
        ans(key) = Map()++t
      }  
      ans
    }
  }


object VolumeMatrix {
  def apply[ A, B](mrp : MatrixResultParameter[A, B, Double] ): 
      (Map[Node, Int], Map[Link, Int]) => MatrixResult.MatrixResultFA[Double]= {
      (NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) =>
        new VolumeMatrix(mrp,NodeToPos, LinkToPos)
            
  }
}


}