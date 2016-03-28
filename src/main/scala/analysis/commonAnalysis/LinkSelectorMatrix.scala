package analysis.commonAnalysis

import bank.Link
import bcfw.FinishingAnalisator
import bank.Node
import analysis.MatrixResult


object LinkSelectorMatrix {
  
  def attribute_filter(links : Set[Link])(link: Link): Boolean = { links.contains(link) }
  
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
  /**
   * creates an analysator that gets the matrix of the volumes that pass through certain links
   * @param links the set of links
   */
  def apply(links : Set[Link]) :  
    (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[Array[Array[(Double, Double)]], scala.collection.Map[Node, Map[Node, Double]]] = {
    MatrixResult(get_nothing _ , attribute_filter(links) _ , filtro _ , evaluator _ , agregator _)
  }
  def apply(links : => Set[Long]) :  
    (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[Array[Array[(Double, Double)]], scala.collection.Map[Node, Map[Node, Double]]] = {
     def init(nodesMap : Map[bank.Node,Int],linksMap : Map[bank.Link,Int]) :  FinishingAnalisator[Array[Array[(Double, Double)]], scala.collection.Map[Node, Map[Node, Double]]]  = {
      val allLinks = linksMap.keySet
      val id2link = allLinks.map { link => (link.id.id , link) }.toMap
      val selectedLinks = links.map { id2link(_) }
      val fun = MatrixResult(get_nothing _ , attribute_filter(selectedLinks) _ , filtro _ , evaluator _ , agregator _)
      fun(nodesMap , linksMap)
     }
     init
  }
}