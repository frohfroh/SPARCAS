package bank

import bcfw.stoppingCriteria
import bcfw.BCFW
import bcfw.Analisator
import bcfw.SimpleAnalisator
import bcfw.FinishingAnalisator
import vdf.VDF
import analysis.SimpleTranslatorFactory
import analysis.TranslatorFactory
import analysis.Translator
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.collection.mutable
import scala.runtime.Tuple2Zipped
import analysis.MatrixResult
import analysis.NetworkResult
import importExport.CSVreader
import java.io.PrintWriter
import java.io.File

object Network{
    trait AssignmentFactory {
    //[T , F , A <: FinishingAnalisator[T , F]]
    def assign[T , F]( analisator : (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[T , F] ) : (Matrix => F)
  }

    //works only with ArrayMatrix in which all zoning systems are exactly the same
  trait Mask {
    val error_weight : Double
    val correct_value : Double
    val weights : Map[(Node,Node) , Double] //peso de cada par nesta screen

    //if the mask is a 
    def value( matrix : Matrix) : Double // gives the value having a matrix 
    /**
     * operates elementwise in-place on a matrix, using op being the matrix the first argument and
     * the value of the mask as the second. Operates only on elements that are non zero os the mask
     */
   // def operate(matrix : ArrayMatrix , op :  (Double,Double) => Double) : Unit
   // def reduceToDouble[T](fs : Double => Double
  }
  //deve realizar duas contas
  //a primeira é dados volumes, retornar o erro
  trait Screen {
    val error_weight : Double //peso desta screen
    val correct_value : Double //valor esperado nesta screen total
    val weights : Map[Link , Double] //peso de cada link nesta screen
    /**
     * given values v_A of doubles for each link the answer will the Double
     *                      Σ w_a * v_a
     *                      a
     * where w_a is the weight of link a on the screen 
     */
    def value( volumes : Map[Link , Double]) : Double 
  }

  
}

//TODO break this class into traits(abstract classes) so to be spread in different files, mostly likely
//1) fields definition 
//2) editing functions extending 1)
//3) assignment functions,Demand adjustment and gross network creation extending 1)
//4) this class extending 2 and 3
// I still haven't completely figured out how. One can see
// http://stackoverflow.com/questions/25545755/is-it-possible-to-split-object-definion-in-several-files-in-scala
// and
// http://stackoverflow.com/questions/3721196/are-there-any-means-in-scala-to-split-a-class-code-into-many-files
//for the general idea
/**
 * This class represents a high-level network which should be edited
 * It has (i)edition methods (ii)assignment operations methods
 *
 *
 *
 */
class Network {
  import Network._
  implicit val thisNetwork = this //in order to create the other elements that point back to this network
  val nodes: collection.mutable.Map[ID, Node] = new HashMap();
  val links: collection.mutable.Map[ID, Link] = new HashMap();
  val centroides: collection.mutable.Map[ID, Node] = new HashMap();
  val NodeAttributes: ArrayBuffer[Attribute[Any, Node]] = new ArrayBuffer(0);
  val LinkAttributes: ArrayBuffer[Attribute[Any, Link]] = new ArrayBuffer(0);
  var usedIDs: Long = 0;
  val vdfAtributes: ArrayBuffer[String] = new ArrayBuffer(0); //Attribute name -> position used in the vdf
  /**
   * Links the id (number visible to the user) with the random so one can recreate the ID
   */
  val IDcatalog: collection.mutable.Map[Long, Long] = new HashMap()
  //Subset takes only the links of a specific mode. 
  //[T , F ,A  <: FinishingAnalisator[T , F]]
  /**
   * performs a biconjugated Frank Wolf asssigment , check BCFW for more information
   */
  def assign[T, F, A](m: Matrix, sc: bcfw.stoppingCriteria, analyser: (Map[Node, Int], Map[Link, Int]) => A, subset: Link => Boolean = (q => true), useConnectors: Boolean = true)(implicit ev: A => FinishingAnalisator[T, F]): F = {
    val (assignment, (nodeToPos, linkToPos)) = grossNetwork(subset)
    assignment.sc = sc
    val matrix: Array[Array[Double]] = m.toArrays(nodeToPos)
    assignment.OD = matrix
    val truAnalyser = analyser(nodeToPos, linkToPos)
    val bruto = assignment.assign(truAnalyser)
    //we have a really weird problem here
    //one would think that ev would be implicitly applied but it is not
    //this question is asked in http://stackoverflow.com/questions/35947230/eclipse-gives-scala-implicit-error-when-used-in-particular-situation
    //please, observe that this use of implicit is already a workaround
    //ideally there would be no implicit argument and the type signature would be [T , F , A <: FinishingAnalisator[T , F]]
    //which works fine here , but scala fails to infer the type at call-site
    //so that every call would have to come with huge type annotations
    //Let us hope that a future scala version will solve one or both problems
    ev(truAnalyser).finalize(bruto)
  }
  /**
   * performs a All or nothing assignment , arguments are the same as in assign
   * @param cost link attribute to be used as cost
   */
  def AllOrNothingAssign[T, F, A](m: Matrix, analyser: (Map[Node, Int], Map[Link, Int]) => A, cost: String, 
      subset: Link => Boolean = (q => true), useConnectors: Boolean = true)(implicit ev: A => FinishingAnalisator[T, F]): F = {
    object uncapacitatedTime extends VDF {
      def attributesNames(): Array[String] = Array(cost)
      def derivative(vol: Double, cost: Array[Double]): Double = 0.0
      def integral(vol: Double, cost: Array[Double]): Double = cost(0) * vol
      def valueAt(vol: Double, cost: Array[Double]): Double = cost(0)
    }
    val (assignment, (nodeToPos, linkToPos)) = grossNetwork(subset, function = {_=> uncapacitatedTime})
    val matrix: Array[Array[Double]] = m.toArrays(nodeToPos)
    assignment.OD = matrix
    //TODO break bcfw.stoppingCriteria into an interface and a class implementing it
    object StopAtZerothIteration extends bcfw.stoppingCriteria(0.0 , 0){
      override def  shouldStop(obtained : stoppingCriteria ) : Boolean = {
        true
      }
    }
    assignment.sc =  StopAtZerothIteration
    val truAnalyser = analyser(nodeToPos, linkToPos)
    val bruto = assignment.assign(truAnalyser)
    ev(truAnalyser).finalize(bruto)
  }

  /**
   * calculates the objective function given a certain link volume
   */
  def objectiveFunction(subset: Link => Boolean = (q => true), useConnectors: Boolean, vols: Map[Link, Double]): Double = {
    val (assignmentz, (aNodeToPosz, aLinkToPosz)) = grossNetwork(subset)
    assignmentz.useConnectors = useConnectors
    val volsLowLevel = vols.map({ case (k, v) => (aLinkToPosz(k), v) }).toArray
    val e = (volsLowLevel.sortBy({ case (k, v) => k })).map(_._2)
    assignmentz.ObjectiveFunction(e)
  }
  /**
   * tranforms this high level network on a BCFW low level network
   * return the low-level network and the maps allowing to go from the elements of this
   * (high-level) network to the elements of the low-level one
   */
  def grossNetwork(subset: Link => Boolean = (q => true),function : Link => VDF =  {l : Link => l.function} ): (BCFW, (Map[Node, Int], Map[Link, Int]) ) = {
    val assignment: BCFW = new BCFW(null);
    assignment.V = nodes.size;
    assignment.Centroides = centroides.size
    val nodesAll = nodes.values.toSet;
    val nodesCentroides = centroides.values.toSet;
    val nodeOrdinary = nodesAll -- nodesCentroides
    val NodeArray: Array[Node] = (nodesCentroides.toArray.sortWith(_ > _) ++ nodeOrdinary.toArray.sortWith(_ > _)) toArray
    val NodeToPos: Map[Node, Int] = NodeArray zip Range(0, NodeArray.size) toMap;  
    val LinkArray: Array[Link] = links.values.filter(subset).toList.sortWith(_ > _).toArray;
    assignment.E = LinkArray.size;
    val LinkToPos = LinkArray zip Range(0, LinkArray.size) toMap;
    val fromNodes: Array[Int] = LinkArray map (link => NodeToPos(link.from));
    val toNodes: Array[Int] = LinkArray map (link => NodeToPos(link.to));
    assignment.fromNode = fromNodes
    assignment.toNode = toNodes
    val exiting: Array[Array[Int]] = NodeArray map (node => (node.exiting map (LinkToPos.getOrElse(_, -1))).toArray) //-1 if node is not connected to the mode
    val arriving: Array[Array[Int]] = NodeArray map (node => (node.arriving map (LinkToPos.getOrElse(_, -1))).toArray)
    assignment.exitingLinks = exiting map (_ filter (_ >= 0)) //remove links that don't have the desired mode
    assignment.arrivingLinks = arriving map (_ filter (_ >= 0))
    val AssignmentAttributes = LinkArray map (link => (function(link).attributesNames() map (link.DoubleAttributes(_))).toArray)
    assignment.linkParameters = AssignmentAttributes
    val vdfs = LinkArray map (function(_))
    assignment.VDFs = vdfs
    (assignment, (NodeToPos, LinkToPos))
  }
  def addCentroid(coords: GeoPos, atts: mutable.Map[String, Any]): Node = {
    val no = addNode(coords, atts)
    centroides += ((no.id, no))
    no
  }
  def addNode(coords: GeoPos, atts: mutable.Map[String, Any]): Node = {
    val id: ID = nextID()
    val atts2 = complement_attributes(atts, NodeAttributes)
    val no = new Node(id, coords, atts2)
    nodes += ((id, no))
    no
  }
  def addLink(shape: Array[GeoPos], from: Node, to: Node, func: VDF, atts: mutable.Map[String, Any]): Link = {
    val id: ID = nextID()
    val atts2 = complement_attributes(atts, LinkAttributes)
    val link = new Link(id, shape, from, to, func, atts2)
    links += ((id, link))
    from.exiting += link
    to.arriving += link
    link
  }
  def complement_attributes[T <: NetworElement](attsValues: mutable.Map[String, Any], atts: Seq[Attribute[Any, T]]): mutable.Map[String, Any] = {
    val thisAtts = attsValues.keySet
    val networkAtts = atts.map(_.Name).toSet
    val undef = thisAtts.--(networkAtts)
    if (!undef.isEmpty) {
      throw new Exception("Attributes not defined: \n" + undef)
    }
    val unvalued = atts.filterNot(att => attsValues.keySet.contains(att.Name))
    val attExtras = unvalued.map(att => (att.Name, att.DefaultValue)).toMap
    attsValues ++ attExtras
  }
  def nextID(): ID = {
    val r = new Random();
    val rand = r.nextLong()
    usedIDs = usedIDs + 1
    IDcatalog(usedIDs) = rand
    ID(usedIDs, rand)
  }
  /**
   * link object from id
   */
  def link = netel[Link](links)_;

  /**
   * node object from id
   */
  def node = netel[Node](nodes)_;

  def netel[T](mapa: scala.collection.Map[ID, T])(id: Long): Option[T] = {
    IDcatalog.get(id) flatMap { a => mapa.get(ID(id, a)) }
  }

  def addNodeAttribute(att: Attribute[Any, Node]) = {
    NodeAttributes += att
    for (node <- nodes) node._2.atts.+=((att.Name, att.DefaultValue))
  }

  def addLinkAttribute(att: Attribute[Any, Link]) = {
    LinkAttributes += att
    for (link <- links) link._2.atts.+=((att.Name, att.DefaultValue))
  }

  def deleteNodeAttribute(attName: String) = {
    for (node <- nodes) node._2.atts - attName
    val pos = NodeAttributes.indexWhere(_.Name equals attName)
    NodeAttributes.remove(pos)
  }
  def deleteLinkAttribute(attName: String) = {
    for (link <- links) link._2.atts - attName
    val pos = LinkAttributes.indexWhere(_.Name equals attName)
    LinkAttributes.remove(pos)
  }
  //TODO should one clean teh IDCatalog? Important for future editing macros
  def deleteNode(id : ID , domino : Boolean = false) ={
    val node : Node =  nodes(id) 
    val links = node.arriving++node.exiting
    if(!links.isEmpty && !domino) throw new Exception("there are links arriving oor leaving the node")
    links.map {_.id }.map{deleteLink(_)}
    nodes -= id 
  }

  def deleteLink(id : ID) = {
    links -= id
  }
  
  
  def trafficDemandAdjustment[A , F](seedMatrix : ArrayMatrix , masks : List[Mask] , screens : List[Screen] , iterations : Int, 
                      assignmentFactory : AssignmentFactory ) : ArrayMatrix = {
    var matrix = seedMatrix
    for(i <- 0 until iterations) matrix =  trafficDemandAdjustmentStep(matrix , masks , screens , assignmentFactory)
    matrix
  }
  //TODO refactor this  function
  //There are quite a few problems with the procedure below
  //First, the matrix are a huge mess, being represented by Array[Array[Double]] , ArrayMatrix , Map[Node,Map[Node,Double]] or Map[(Node,Node) , Double]
  //this is in part because the matrix definitions are a mess in the bank itself
  //Secondly , Masks and Screens present a beautiful duality , but this is not explored enough
  //one can see some use at IndexedVectorG but even this case could maybe be simplified
  //The two classes themselves are very close one to each other
  //atribute_from_screens and matrix_from_masks are virtually the same function written twice
  //Thirdly both matrix and vectors calculations are horrible, using some utility op and op2 and lots of maps and zip/zipped
  //using   breeze can help
  //Finally, one should worry about performance
  //In a typical application, one would have a small numbers os screens (dozens) each one having its support on very few links(mostly one or two (express and local lanes))
  //It is not impossible though that one choose to use it as a real screenline composed of dozens of links
  //In any of these cases there should be no serious problems
  //This is not the same for then Masks because in a typical case one would have zones^2 masks each one with only one non null cell
  //but one may also want to fix vectors or even the matrix as a whole, so we can have quite a few masks with many cells in each one
  //all of this may be alleviated by the fact that we run this only one per each assignment , so its costs may be dwarfed by the assignment time
  def trafficDemandAdjustmentStep[A , F](matrix : ArrayMatrix , masks : List[Mask] , screens : List[Screen] ,  
                      assignmentFactory : AssignmentFactory ) : ArrayMatrix = {
    
    def op2( m1 : Array[Array[Double]] , m2 : Array[Array[Double]] , f : (Double,Double) => Double) : Array[Array[Double]]= {
      val int1 = (m1,m2).zipped 
      val int2 = int1.map((_,_).zipped) 
      int2.map{x => x.map{f}}
    }
    def op( m : Array[Array[Double]] ,  f : Double => Double) = {
       m.map { linha => linha.map{f} }
    }
    implicit def  matrix2AOA(m : ArrayMatrix) = m.raw
    implicit def  AOA2matrix(m : Array[Array[Double]]) =  new ArrayMatrix(m , matrix.mapa)
    implicit def  MOM2AOA(m :  scala.collection.Map[Node, scala.collection.Map[Node, Double]]) = {
      val n = matrix.raw.length
      val aoa = Array.fill(n, n)(0.0)
      for((nf,vec) <- m){
        for((nt,v) <- vec){
          aoa(matrix.Node2Int(nf))(matrix.Node2Int(nt)) = v
        }
      }
      aoa
    }
    implicit def  MNN2AOA(m :  scala.collection.Map[(Node,Node), Double]) = {
      val n = matrix.raw.length
      val aoa = Array.fill(n, n)(0.0)
      for(((nf,nt),v) <- m){
          aoa(matrix.Node2Int(nf))(matrix.Node2Int(nt)) = v
      }
      aoa
    }
    //I am not completely sure if I can make this class implicit and not create the two others specializing it
    //However , even if this is allowed in scala, it may slow a lot the compiler
    //Even if the compiler can handle it well , defining the two classes explicitly makes thing clearer
    //One could argue that even better would be to use this class explicitly avoid implicits
    //I believe the more complex parts of the code get simpler by using things that way
    class IndexedVectorG[U](l : Map[U ,  Double] ) {
      def +(r : Map[U ,  Double]) :  Map[U ,  Double] ={
        val links = l.keySet++r.keySet
        val pairs = links.map { lk => (lk,l.getOrElse(lk, 0.0)+r.getOrElse(lk, 0.0)) }
        pairs.toMap
      }
      def *(m : Double) :  Map[U ,  Double] ={
        l.map(p =>(p._1 , p._2 * m)).toMap
      }
      def sum() : Double = l.values.sum
    }
    implicit class IndexedVectorM(l : Map[(Node,Node) ,  Double] ) extends IndexedVectorG[(Node,Node)](l)
    implicit class IndexedVector(l : Map[Link ,  Double] ) extends IndexedVectorG[Link](l)
    //TODO eliminate this class below
    /*implicit class IndexedVector(l : Map[Link ,  Double] ) {
      def +(r : Map[Link ,  Double]) :  Map[Link ,  Double] ={
        val links = l.keySet++r.keySet
        val pairs = links.map { lk => (lk,l.getOrElse(lk, 0.0)+r.getOrElse(lk, 0.0)) }
        pairs.toMap
      }
      def *(m : Double) :  Map[Link ,  Double] ={
        l.map(p =>(p._1 , p._2 * m)).toMap
      }
      def sum() : Double = l.values.sum
    }*/
    /**
     * given values v_s of doubles for each screen the answer will be a link attribute 
     * such that its value on link a is Σ w_s_a * v_s 
     *                                  s
     * where w_s_a is the weight of link a on the screen s
     */
    def atribute_from_screens( d_s : List[(Double ,Screen)]  ) : Map[Link ,  Double] = {
      val per_srn = d_s.map({case (v,w) => w.weights*v})
      per_srn.reduce(_ + _)
    }
    /**
     * given values v_m of doubles for each mask the answer will be a matrix
     * such that its value on cell i is Σ g_m_i * v_m 
     *                                  m
     * where g_m_i is the weight of cell i on the mask m 
     */
    def matrix_from_masks( v_m : List[(Double ,Mask)]  ) : scala.collection.Map[(Node,Node) ,  Double] = {
      val per_srn = v_m.map({case (v,w) => w.weights * v})
      println("performing matrix_from_masks")
      val ans  = scala.collection.mutable.Map[(Node,Node) ,  Double]()
      for( mapa <- per_srn){
        for( (nono,v) <- mapa) ans(nono) = ans.getOrElse(nono, 0.0)+v 
      }
      ans
      //the simples solution bellow is not acceptable due to performance issues
      //profiling traffic adjustment as a whole is an important task, preferentially after a good factoring
      //per_srn.reduce(_ + _)
    }    
    
    

    //First assignment
    val va = assignmentFactory.assign(analysis.LinksVolumesDur.apply _ )(matrix) // alocação de matriz
    val di = matrix
    

    val SC2 = screens.map { _.value(va) }
    val SC3 = screens.map { _.correct_value }
    val SC4 = (SC2, SC3).zipped.map(_ - _)
    val α   = screens.map { _.error_weight } 
    

    
    val SC9p = screens.map { sc => 2 * sc.error_weight   }
    val SC9 = SC9p.zip(SC4).map(l => l._1 * l._2)// s -> 
    
    val SC10 = atribute_from_screens(SC9.zip(screens))
    
    
    //Second Assignment
    def agregator(path1: (Double, Double), path2: (Double, Double)): (Double, Double) = {
      (path1._1 + path2._1, path1._1 * path1._2 + path2._1 * path2._2)
    }
    def get_nothing(link: Link): Unit = ()
    def get_SC10(link : Link) : Double = SC10.getOrElse(link , 0.0) //SC10 will only have keys for links that are at least in ine screen
    def filter(s: Seq[Unit]): Boolean = true
    def evaluator(algo: Seq[Double]): Double = algo.sum
    val matrix_skimmer = MatrixResult(get_nothing _ ,get_SC10 _,  filter, evaluator, agregator)
  
    val dZddiG : Array[Array[Double]]  =  assignmentFactory.assign(matrix_skimmer)(matrix)// alocação de matriz//skim de SC10  
    

    
    val SC6 = masks.map { _.value(di) }
    val SC7 = masks.map { _.correct_value }
    val SC8 = (SC6,SC7).zipped.map(_ - _)  // m ->
   


    
    val SC11 = masks.zip(SC8).map(mv => 2 * mv._1.error_weight * mv._2)  //
    

    
    val dZddiD : Array[Array[Double]] =  matrix_from_masks(SC11.zip(masks));
    


    
    val dZddi : Array[Array[Double]] = op2(dZddiG , dZddiD , {_+_}) //Soma dos termos esquerdo e direito
    
    


        val dì = op2(di , dZddi , {- _ * _}) //di/dλ = - di * (dZ / ddi) = - di * dZddi

    //Third Assignment
    val slave_assigner = NetworkResult.create(get_nothing , filter , Some(dì))
    val và = assignmentFactory.assign(slave_assigner)(matrix) // alocação de matriz // alocação de - di * dZddi = dì

       val postos_ctg = List((618,617),(453,454),(729,730)).flatMap { x => links.values.find { l => l.atts("From")==x._1 && l.atts("To")==x._2 } }


    val SC1 = screens.map { _.value(và) }

    
    
    val NG = (α , SC4 , SC1).zipped.map{_ * _ * _}.sum
    val DG = (α , SC1 , SC1).zipped.map{_ * _ * _}.sum
    
    
    

    
    val SC5 = masks.map { _.value(dì) } // masks.map would be replaced
    val β   = masks.map { _.error_weight } 

    val ND = (β , SC8 , SC5).zipped.map{_ * _ * _}.sum
    val DD = (β , SC5 , SC5).zipped.map{_ * _ * _}.sum
    
    


    val λ = - (NG+ND)/(DG+DD)
    
                                        println("\n\n\n trafficDemandAdjustment λ "+λ+"\n\n\n")

    val left = op(dZddi,{1 - λ * _} )
    
    val dλ = op2(di,left,_ * _)
    
    new ArrayMatrix(dλ , matrix.mapa)
  }
  
  
}