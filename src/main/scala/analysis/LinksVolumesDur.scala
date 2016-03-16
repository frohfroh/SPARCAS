package analysis

import bcfw.FinishingAnalisator
import bank.Node
import bank.Link

object LinksVolumeDurFactory extends TranslatorFactory[Array[ Double] , Map[Link,Double] , LinksVolumesDur]{
  def createFinishingAnalyser(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]): LinksVolumesDur = {
    new LinksVolumesDur(NodeToPos, LinkToPos) 
  }
}
case class LinksVolumesDur(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) extends FinishingAnalisator[Array[ Double] , Map[Link,Double]] { 
	def  simplePath(path_matrix : Array[Array[Array[Int]]]  , matriz_od : Array[Array[Double]] ) : Array[ Double]={
	   // path_matrix is such that path_matrix[a][b][c] is the c-th link on the path from node a to node b
	  val vols = new Array[Double](LinkToPos.size)
	  val zones = path_matrix.size
	  for(from <- 0 until zones){
	    for(to <- 0 until zones){
	      for(link <- 0 until path_matrix(from)(to).size){
	        vols(path_matrix(from)(to)(link)) += matriz_od(from)(to)
	      }
	    }
	  }
	  return vols
	}
	def  combine(left : Array[ Double] ,right : Array[ Double] , left_proportion : Double ) : Array[ Double] = {
	  val righ_proportion = 1-left_proportion
	  val vols = new Array[Double](LinkToPos.size)
	  for(i <- 0 until vols.size){
	    vols(i) = left(i) * left_proportion +  right(i) * righ_proportion
	  }
	  vols
	}
	def finalize(brute : Array[ Double] ) : Map[Link,Double] = {
	  val PosToLink = LinkToPos.map({case (a,b) => (b,a)}).toMap  
	  (0 until brute.size).map(v => (PosToLink(v),brute(v))).toMap
	}

}