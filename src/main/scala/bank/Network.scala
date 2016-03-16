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


/**
 * This class represents a high-level network which should be edited
 * It has (i)edition methods (ii)assignment operations methods
 * 
 * 
 * 
 */
class Network {
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
  val IDcatalog : collection.mutable.Map[Long, Long] = new HashMap()
  //Subset takes only the links of a specific mode. this will be rewritten together with attributes
 //[T , F ,A  <: FinishingAnalisator[T , F]]
  /**
   * performs a biconjugated Frank Wolf asssigment , check BCFW for more information
   */
  def assign[T , F , A  <: FinishingAnalisator[T , F]](m: Matrix, sc: bcfw.stoppingCriteria, analyser: TranslatorFactory[T,F, A], subset: Link => Boolean = (q => true) , useConnectors : Boolean = true): F = {
    
    val (assignment , (nodeToPos,linkToPos)) = grossNetwork(subset)
    assignment.sc = sc
 
    val matrix: Array[Array[Double]] = m.toArrays(nodeToPos)
    assignment.OD = matrix
    val truAnalyser = analyser.createFinishingAnalyser(nodeToPos, linkToPos)
    val bruto = assignment.assign(truAnalyser)
    truAnalyser.finalize(bruto)
  }
  //TODO delete assign and substitute it everywhere by assign2
  //after that get rid of all those awfull Factory methods like LinksVolumeDurFactory et al
  def assign2[T , F , A ](m: Matrix, sc: bcfw.stoppingCriteria, analyser: (Map[Node, Int] , Map[Link, Int]) => A, subset: Link => Boolean = (q => true) , useConnectors : Boolean = true)(implicit ev :  A => FinishingAnalisator[T , F]): F = {
    val (assignment , (nodeToPos,linkToPos)) = grossNetwork(subset)
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
   * calculates the objective function given a certain link volume
   */
  def objectiveFunction(subset: Link => Boolean = (q => true) , useConnectors : Boolean ,vols : Map[Link,Double]) : Double = {
    val (assignmentz , (aNodeToPosz,aLinkToPosz)) = grossNetwork(subset)
    assignmentz.useConnectors = useConnectors
    val volsLowLevel = vols.map({case (k,v) => (aLinkToPosz(k),v)}).toArray
    val e = (volsLowLevel.sortBy({case (k,v) => k})).map(_._2)
    assignmentz.ObjectiveFunction(e)
  }
  /**
   * tranforms this high level network on a BCFW low level network
   * return the low-level network and the maps allowing to go from the elements of this 
   * (high-level) network to the elements of the low-level one
   */
  def grossNetwork( subset: Link => Boolean = (q => true)) : (BCFW ,(Map[Node, Int] , Map[Link, Int] ))= {
    val assignment: BCFW = new BCFW(null);
    assignment.V = nodes.size;
    assignment.Centroides = centroides.size
    val nodesAll = nodes.values.toSet;
    val nodesCentroides = centroides.values.toSet;
    val nodeOrdinary = nodesAll -- nodesCentroides
    val NodeArray: Array[Node] = (nodesCentroides.toArray.sortWith(_>_) ++ nodeOrdinary.toArray.sortWith(_>_)) toArray
    val NodeToPos: Map[Node, Int] = NodeArray zip Range(0, NodeArray.size) toMap;
    val LinkArray : Array[Link] =links.values.filter(subset).toList.sortWith(_ > _).toArray;
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
    val AssignmentAttributes = LinkArray map (link => (link.function.attributesNames() map (link.DoubleAttributes(_))).toArray)
    assignment.linkParameters = AssignmentAttributes
    val vdfs = LinkArray map (_.function)
    assignment.VDFs = vdfs
    (assignment , (NodeToPos ,LinkToPos))
  }
  def addCentroid(coords: GeoPos, atts: mutable.Map[String, Any]): Node = {
    val no = addNode(coords, atts)
    centroides += ((no.id, no))
    no
  }
  def addNode(coords: GeoPos, atts: mutable.Map[String, Any]): Node = {
    val id: ID = nextID()
    val atts2 = complement_attributes(atts , NodeAttributes)
    val no = new Node(id, coords, atts2)
    nodes += ((id, no))
    no
  }
  def addLink(shape: Array[GeoPos], from: Node, to: Node, func: VDF, atts: mutable.Map[String, Any]): Link = {
    val id: ID = nextID()
    val atts2 = complement_attributes(atts , LinkAttributes)
    val link = new Link(id, shape, from, to, func, atts2)
    links += ((id, link))
    from.exiting += link
    to.arriving += link
    link
  }
  def complement_attributes[T <: NetworElement](attsValues: mutable.Map[String, Any] , atts : Seq[Attribute[Any, T]] ) : mutable.Map[String, Any] = {
    val thisAtts = attsValues.keySet
    val networkAtts = atts.map(_.Name).toSet
    val undef = thisAtts.--(networkAtts)
    if(!undef.isEmpty){
      throw new Exception("Attributes not defined: \n"+undef)
    }
    val unvalued = atts.filterNot(att => attsValues.keySet.contains(att.Name))
    val attExtras = unvalued.map(att => (att.Name,att.DefaultValue)).toMap
    attsValues++attExtras
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
  

  def netel[T](mapa : scala.collection.Map[ID , T])(id : Long ) : Option[T] = {
    IDcatalog.get(id) flatMap {a => mapa.get(ID(id , a))}
  }
  
  def addNodeAttribute(att  : Attribute[Any,Node]) = {
    NodeAttributes += att
    for(node <- nodes) node._2.atts.+=((att.Name,att.DefaultValue))
  }
  
  def addLinkAttribute(att  : Attribute[Any,Link]) = {
    LinkAttributes += att
    for(link <- links) link._2.atts.+=((att.Name,att.DefaultValue))
  }
  
  def deleteNodeAttribute(attName : String) = {
    for(node <- nodes) node._2.atts - attName
    val pos = NodeAttributes.indexWhere(_.Name equals attName)
    NodeAttributes.remove(pos)
  }
  def deleteLinkAttribute(attName : String) = {
    for(link <- links) link._2.atts - attName
    val pos = LinkAttributes.indexWhere(_.Name equals attName)
    LinkAttributes.remove(pos)
  }
}