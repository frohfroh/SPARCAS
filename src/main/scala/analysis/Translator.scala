package analysis

import bcfw.SimpleAnalisator
import bcfw.FinishingAnalisator

import bank.Matrix
import bank.Node
import bank.Link
import bank.ArrayMatrix


trait TranslatorFactory[T , F ,S  <: FinishingAnalisator[T , F]]{
  def createFinishingAnalyser(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]): S;
  
}

trait SimpleTranslatorFactory[T, S <: SimpleAnalisator[T]] extends  TranslatorFactory[T , T ,S]{
  
  def createFinishingAnalyser(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]): S = {
     createAnalyser(NodeToPos, LinkToPos)
  }
  
  def createAnalyser(NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]): S;
}
/***
 * Allows simply that the operations be defined in terms of objects and not vectors
 */
abstract class Translator[T](NodeToPos: Map[Node, Int], LinkToPos: Map[Link, Int]) extends SimpleAnalisator[T] {
  val PosToNode: Map[Int, Node] = NodeToPos.map(_.swap)
  val PosToLink: Map[Int, Link] = LinkToPos.map(_.swap)
 
  def simplePath2(caminhos: Map[(Node, Node), Array[Link]], mat: Matrix): T;
  /***
   * 	 path_matrix is such that path_matrix[a][b][c] is the c-th link on the path from node a to node b
   */
  override def simplePath(path_matrix: Array[Array[Array[Int]]], matriz_od: Array[Array[Double]]): T = {
    val algo = for (c1 <- 0 until path_matrix.length; c2 <- 0 until path_matrix(c1).length) 
      yield ((PosToNode(c1), PosToNode(c2)), path_matrix(c1)(c2) map (PosToLink(_)))
    val objectPath: Map[(Node, Node), Array[Link]] = algo toMap
    val objectMatrix = new ArrayMatrix(matriz_od, PosToNode)
    simplePath2(objectPath, objectMatrix)
  }
}