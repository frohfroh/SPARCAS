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
import breeze.linalg.HashVector
import scala.reflect.ClassTag
import breeze.storage.Zero
import analysis.BreezeMatrixResultParameter
import analysis.BreezeMatrixResult



object Network{
    trait AssignmentFactory {
    //[T , F , A <: FinishingAnalisator[T , F]]
    def assign[T , F]( analisator : (Map[Node, Int], Map[Link, Int]) => FinishingAnalisator[T , F] ) : (Matrix[Double] => F)
  }

    //TODO breezefy this 
    //works only with ArrayMatrix in which all zoning systems are exactly the same
  trait Mask {
    val error_weight : Double
    val correct_value : Double
    val weights : Map[(Node,Node) , Double] //peso de cada par nesta screen

    //if the mask is a 
    def value( matrix : Matrix[Double]) : Double // gives the value having a matrix 
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
     * given values v_A of doublfes for each link the answer will the Double
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
  implicit val thisNetwork = this //in order to create the other elements
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
  def assign[T, F, A](m: Matrix[Double], sc: bcfw.stoppingCriteria, analyser: (Map[Node, Int], Map[Link, Int]) => A, subset: Link => Boolean = (q => true), useConnectors: Boolean = true)(implicit ev: A => FinishingAnalisator[T, F]): F = {
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
  def AllOrNothingAssign[T, F, A](m: Matrix[Double], analyser: (Map[Node, Int], Map[Link, Int]) => A, cost: String, 
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
  
  
    
  
  def trafficDemandAdjustment[A , F](seedMatrix : Matrix[Double] , masks : List[Mask] , screens : List[Screen] , iterations : Int, 
                      assignmentFactory : AssignmentFactory ) : Matrix[Double] = {
    var matrix = seedMatrix
    for(i <- 0 until iterations){
      val (left_error , right_error , temp_matrix , va) = trafficDemandAdjustmentStep4(matrix , masks , screens , assignmentFactory)
      println(" ERRO NA ITERAÇÃO "+i+" : fov "+left_error+" matriz "+right_error+" conjunto "+(left_error + right_error))
      matrix = temp_matrix
    }
    matrix
  }
  

  
 def canonicalLinkEnumeration = {
   //val links: collection.mutable.Map[ID, Link] = new HashMap();
    val LinkArray: Array[Link] = links.values.toList.sortWith(_ > _).toArray;
    val linkToPos = LinkArray zip Range(0, LinkArray.size) toMap;
    val posToLink = linkToPos.map(_.swap).toMap
    (linkToPos,posToLink)
 }
 def canonicalCentroidEnumeration = {
    val nodesCentroides = centroides.values.toSet;
    val NodeArray: Array[Node] = (nodesCentroides.toArray.sortWith(_ > _) ) toArray
    val centroidToPos = NodeArray zip Range(0, NodeArray.size) toMap;
    val posToCentroid = centroidToPos.map(_.swap).toMap
    (centroidToPos,posToCentroid)
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

 def trafficDemandAdjustmentStep4[A , F](matrixWO : Matrix[Double] , masks : List[Mask] , screens : List[Screen] ,  
                      assignmentFactory : AssignmentFactory ,  firstAssignment : Option[Map[Link, Double]] = None) :(Double, Double, Matrix[Double], Map[Link, Double]) = {
    import breeze.{linalg => bz}
    
    val (centroidToPos,posToCentroid) = canonicalCentroidEnumeration

    val matrixBruta = matrixWO.toArrays(centroidToPos) 
    val matrix = new ArrayMatrix( matrixBruta , posToCentroid)
    

    //First assignment
    val va = firstAssignment match {
      case Some(fad) => fad
      case None => assignmentFactory.assign(analysis.LinksVolumesDur.apply _ )(matrix) // alocação de matriz
    }
 
    val di = matrix
    val diV2 = BreezeMatrix(matrix)
    
    println("O total da matriz recebida é "+bz.sum(diV2.bm))
    
    
    val screensV = bz.DenseVector(screens.toArray)
    val masksV = bz.DenseVector(masks.toArray)
    
    val (linkToPos,posToLink) = canonicalLinkEnumeration
    //val (centroidToPos,posToCentroid) = canonicalCentroidEnumeration
   
    val diV = diV2.toBreeze(centroidToPos)
    val screensWeightsV = screensV.map { _.weights }
    val masksWeightsV = masksV.map { _.weights }

    val num_links = linkToPos.size
    val num_centroids = centroidToPos.size
    def mapLinktoHashVector[T : ClassTag:Zero ](mmap : Map[Link , T]) = {
      val hv = bz.HashVector.zeros[T](num_links)
      for((l,v) <- mmap) hv(linkToPos(l)) = v
      hv
    }
    /*def hashVectortoMapLink[T : ClassTag:Zero ](hv : HashVector[T]) = {
      ((hv.activeIterator).map({case(k,v) => (posToLink(k) , v)})).toMap
    }*/
    def mapNodeNodetoHashMatrix[T : ClassTag:Zero ](mmap : Map[(Node,Node) , T]) = {
      val hm = bz.HashMatrix.zeros[T](num_centroids,num_centroids)
      for(((nf,nt),v) <- mmap) hm(centroidToPos(nf),centroidToPos(nt)) = v
      hm
    }
    val screensWeightsV2 = screensWeightsV.map(mymap => mapLinktoHashVector(mymap))
    val masksWeightsV2 = masksWeightsV.map(mymap => mapNodeNodetoHashMatrix(mymap))//matrix *1.0 aumenta
    

    /**
     * given values v_s of doubles for each screen the answer will be a link attribute 
     * such that its value on link a is Σ w_s_a * v_s 
     *                                  s
     * where w_s_a is the weight of link a on the screen s
     */
    def atribute_from_screensV[T <: bz.Vector[Double] : ClassTag]( d : bz.DenseVector[Double] ,s : bz.DenseVector[T] ) : bz.Vector[Double] = {
      val dts = (d zip s) mapValues (ds => ds._2 * ds._1)
      dts.reduce(_+_)
    }
    //We would like to have T <: bz.Matrix[Double] but unfourtunately if we do so
    //the implict found for the "* Scalar" is the Matrix and not the specialized HashMatrix,DenseMatrix, etc. one
    //what destroys our matrixes sparcity one first multiplication
    //this unfourtunately pretty much renders using breeze useless
    //but due to the invested time and lack of better library, will be kept for a while
    /**
     * given values v_m of doubles for each mask the answer will be a matrix
     * such that its value on cell i is Σ g_m_i * v_m 
     *                                  m
     * where g_m_i is the weight of cell i on the mask m 
     */
    def matrix_from_masksV[T <: bz.HashMatrix[Double] : ClassTag]( d : bz.DenseVector[Double] ,s : bz.DenseVector[T] ) : bz.Matrix[Double] = {
      val dts = (d zip s) mapValues (ds => ds._2 * ds._1)
      dts.reduce(_+_)
    }
    
    val SC2V = screensV.map { _.value(va) }
    
    println("SC2V "+ SC2V)
    
    val SC3V = screensV.map { _.correct_value }
    val SC4V = SC2V - SC3V
    val αV   = screensV.map { _.error_weight } 

        
    val SC9V =  αV *:*  SC4V * 2.0
    
    val SC10V = atribute_from_screensV(SC9V,screensWeightsV2)
    
    

    
    //Second Assignment
    def agregator(path1: (Double, Double), path2: (Double, Double)): (Double, Double) = {
      (path1._1 + path2._1, path1._1 * path1._2 + path2._1 * path2._2)
    }
    def get_nothing(link: Link): Unit = ()
    def get_SC10V(link : Link) : Double =SC10V.apply(linkToPos(link)) //SC10 will only have keys for links that are at least in one screen
    def filter(s: Seq[Unit]): Boolean = true
    def evaluator(algo: Seq[Double]): Double = algo.sum
    

    val analysis_paramV :BreezeMatrixResultParameter[Unit, Double, Double]= BreezeMatrixResultParameter( get_nothing _, get_SC10V _, filter, evaluator, agregator) 
   
    val matrix_skimmerV = BreezeMatrixResult.foo(analysis_paramV)//TODO rename foo
    
    val dZddiGV  : BreezeMatrix[Double, bz.DenseMatrix[Double]] =  assignmentFactory.assign(matrix_skimmerV)(matrix)
    
    
    val SC6V = masksV.map { _.value(di) }
    val SC7V = masksV.map { _.correct_value }
    val SC8V = SC6V - SC7V
    val βV   = masksV.map { _.error_weight } 
    
    val SC11V = βV *:* SC8V * 2.0
    
    

    val dZddiDV = matrix_from_masksV[bz.HashMatrix[Double]](SC11V,masksWeightsV2)

    val dZddiV = dZddiGV.toBreeze(centroidToPos)+dZddiDV.toDenseMatrix
    
    val dìV = - diV *:* dZddiV
    
    

    //Third Assignment
    
    val slave_assignerV = NetworkResult.create(get_nothing , filter , Some(new BreezeMatrix(dìV , posToCentroid)))
    val vàV2 = assignmentFactory.assign(slave_assignerV)(matrix) // alocação de matriz // alocação de - di * dZddi = dì
    val vàV = mapLinktoHashVector(vàV2)
        
    
    
    val SC1V = screensV.map { _.value(vàV2) }
    
    val NGV =  bz.sum(αV *:* SC4V *:* SC1V)
    val DGV =  bz.sum(αV *:* SC1V *:* SC1V)
    

    //TODO some implicits to ease this ?
    val SC5V = masksV.map { _.value(new BreezeMatrix(dìV , posToCentroid)) }
    
    val NDV = bz.sum(βV *:* SC8V *:* SC5V)
    val DDV = bz.sum(βV *:* SC5V *:* SC5V)
       
    
    val λ = - (NGV+NDV)/(DGV+DDV)

 
                                        println("\n\n\n trafficDemandAdjustment λ "+λ+" "+"\n\n\n")//comparados e iguais

    val leftV = 1.0  - (λ *    dZddiV) 
    
    val dλV = diV *:* leftV
    
    
    val ansfinal : Matrix[Double]= new BreezeMatrix(dλV , posToCentroid)
    
    //Cálculo de Z
    //termo esquerdo:
     val left_error = bz.sum(αV *:* SC4V *:*  SC4V) 
     println("erro das cortinas:"+SC4V)
     val right_error = bz.sum(βV *:* SC8V *:*  SC8V)
     val error_before = left_error + right_error
     println("o erro antes era de observado "+left_error+" matriz "+right_error+" total "+error_before)
       
 
     println("O total da matriz produzida é "+bz.sum(dλV))

     //Fourth Assignment
     
     //This assignment is not used in the classic adjustment 
     //The problem is that due to differences between the linearized version and the true assignment
     //sometimes we increase rather then decrease ther error
     //this create convergence problems, not only rendering it much slower but sometimes preventing it altogether
     //So a final step is performed. We assign and if the error is increased, we reduce λ till it decreases
     //This last assignment is identical to the first assignment of the next iteration
     //So we return this value to be reused . If it is the last iteration, it is possibly wasted, but at least we the correct errors
     //It would also be a possibility to return the assignment, as most likely he who adjusted would want this values anyway
     var nλ = λ 
     val vaV = mapLinktoHashVector(va)

     while(nλ > λ/(144.0*12.0)){
         //Here, supposing that flows will be linear with the demand, wen get the corresponding values with the new demand, without a new assignment
        val leftV = 1.0  - (nλ *    dZddiV) 
        val dλV = diV *:* leftV
        val dlam =  new BreezeMatrix[Double ,bz.DenseMatrix[Double]] (dλV,posToCentroid)
      
        val vaλ = assignmentFactory.assign(analysis.LinksVolumesDur.apply _ )(dlam)
        val SC2VSupposed = screensV.map { _.value(vaλ) }    
        val SC4VSupposed = SC2VSupposed - SC3V
        val SC6VSupposed = masksV.map { _.value(dlam) } //this is not supposed , but precise
        val SC8VSupposed = SC6VSupposed - SC7V
        val new_left_error = bz.sum(αV *:* SC4VSupposed *:*  SC4VSupposed) 
        val new_right_error = bz.sum(βV *:* SC8VSupposed *:*  SC8VSupposed)
        println("usando nλ "+nλ+ " já com "+λ/nλ)
            println("o erro novo é  observado "+new_left_error+" matriz "+new_right_error+" total "+(new_left_error+new_right_error))
        if(new_left_error+new_right_error < error_before) return (new_left_error , new_right_error , dlam , vaλ)
        else nλ = nλ / 2.0
     }
    //dans le cas ou ça ne converge plus, les critères de convergence de l'affectation en sont possiblement les culpables
  (left_error , right_error , matrix , va)

  }
}