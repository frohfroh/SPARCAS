package analysis.commonAnalysis

import bank.Link
import bcfw.FinishingAnalisator
import analysis.NetworkResult

/**
 * creates a link selector analysator that returns the volumes of paths passing through any of the selected links
 */
object LinkSelectorNetwork {
  
  
  def attribute(links : Set[Link])(link: Link): Boolean = { links.contains(link) }
  
  def filter(s: Seq[Boolean]): Boolean = s.exists(x => x)
  /**
   * creates an anaysator that produces the volume on links of paths using a specific Set of links
   * @param links the set of links
   */
  def apply(links : Set[Link] ):   (Map[bank.Node,Int], Map[bank.Link,Int]) ⇒  FinishingAnalisator[Array[Double], Map[Link, Double]] =apply(links , None)
  
    /**
   * creates an anaysator that produces the volume on links of paths using a specific Set of links
   * @param links the set of links
   * @slaveDemand the volume to be used for the select link, keeping the paths of the demand of the assignment 
   */
  def apply(links : Set[Link] , slaveDemand: Option[Array[Array[Double]]]) :   (Map[bank.Node,Int], Map[bank.Link,Int]) ⇒  FinishingAnalisator[Array[Double], Map[Link, Double]] = {
     NetworkResult(attribute(links) _ , filter _, slaveDemand)
  }
  
  /**
 * creates an anaysator that produces the volume on links of paths using a specific Set of links
 * @param links the set of the id numbers of the links
 */
 def apply(links : => Set[Long] ):   (Map[bank.Node,Int], Map[bank.Link,Int]) ⇒  FinishingAnalisator[Array[Double], Map[Link, Double]] =apply(links , None) 
 
    /**
   * creates an anaysator that produces the volume on links of paths using a specific Set of links
   * @param links the set of the id numbers of the links
   * @slaveDemand the volume to be used for the select link, keeping the paths of the demand of the assignment 
   */
 def apply(links : => Set[Long] , slaveDemand: Option[Array[Array[Double]]]) :   (Map[bank.Node,Int], Map[bank.Link,Int]) ⇒  FinishingAnalisator[Array[Double], Map[Link, Double]] = {
   def init(nodesMap : Map[bank.Node,Int],linksMap : Map[bank.Link,Int]) :  FinishingAnalisator[Array[Double], Map[Link, Double]] = {
      val allLinks = linksMap.keySet
      val id2link = allLinks.map { link => (link.id.id , link) }.toMap
      val selectedLinks = links.map { id2link(_) }
      val fun = NetworkResult(attribute(selectedLinks) _ , filter _, slaveDemand) 
      fun(nodesMap , linksMap)
   }
   init
  } 
  
}