package example

import bank.Network
import bank.Attribute
import bank.Link
import collection.mutable.Map
import bank.ArrayMatrix
/**
 * shows a very simple example of how to define a network, a matrix,
 * perform an assignment and get the result
 *
 * Our network is composed of only two centroids : Source and Sink
 * The matrix is simply:
 *
 *              Source       Sink
 *    Source    0.0          1.0
 *    Sink      0.0          0.0
 *
 *    and the network is composed of 4 ways to go from Source to Sink:
 *    Upper, Upper_Middle , Lower_Middle and Lower
 *    4 additional nodes are created in order to be the middle points of this paths,
 *    composed each of two links : (Source -> Middle) and (Middle -> Sink)
 *
 *    The first link of each path has a Volume delay function that are BPR functions that is
 *    T = T0 * (1 + α * ( (v / c) ** β))
 *    where:
 *    	T  : congested time
 *    	T0 : free-flow time
 *    	α  : constant
 *    	β  : constant
 *      c  : constant (normally the capacity)
 *      v  : volume of the flow
 *
 *    In our example we have the following constants
 *    Upper        : T0 = 1.0 ; α = 0.15 ;  β = 4.0  ;  c = 0.2
 *    Upper-Middle : T0 = 2.0 ; α = 0.15 ;  β = 4.0  ;  c = 0.5
 *    Lower-Middle : T0 = 0.5 ; α = 0.15 ;  β = 20.0 ;  c = 0.1
 *    Lower        : T0 = 1.0 ; α = 0.15 ;  β = 4.0  ;  c = 0.3
 *
 *    The second link of each of the four paths is simply a constant time link
 *    T = 1.0
 *
 *    This 4 intermediate nodes and first and second link per path were created in order
 *    to connect the Source and Sink without having two links with the same origin and destination nodes
 *
 *
 */
object FourPaths extends App {
  //we first create a new Network
  //the constructor take no arguments, everything is filled later
  val net: Network = new Network

  //we now define the attributes of the network
  //We need for every link the four  constants T0 , α , β and c
  //We proceed in two steps (i) create an Attribute object (ii) insert it into the network

  //(i) create an Attribute object
  //we use the Attribute class . Attributes can be of any type, but only Doubles can be used in the assignment
  //Every attribute has a name and a default value

  val T0 = Attribute[Double, Link](0.0, "T0")
  val α = Attribute[Double, Link](0.0, "α")
  val β = Attribute[Double, Link](0.0, "β")
  val c = Attribute[Double, Link](0.0, "c")

  //We add a name of type String as well

  val linkName = Attribute[String, Link]("unnamed", "name")

  //(ii) insert it into the network

  net.addLinkAttribute(T0)
  net.addLinkAttribute(α)
  net.addLinkAttribute(β)
  net.addLinkAttribute(c)
  net.addLinkAttribute(linkName)

  //We now define the nodes of the network
  //Every node has a position given by a GeoPos object and attributes
  //Currently GeoPos is not implemented, so we pass null
  //As we don't have any node attributes, we pass an empty map
  //first the two centroids

  val Source = net.addCentroid(null, Map())
  val Sink = net.addCentroid(null, Map())

  //now the four intermediate nodes

  val Unode = net.addNode(null, Map())
  val UMnode = net.addNode(null, Map())
  val ULnode = net.addNode(null, Map())
  val Lnode = net.addNode(null, Map())

  //We now define the links of the network
  //in order to add links, one must provide their 
  //volume delay function , from node, to node and attributes
  //the volume delay function are defined in their files BRP.scala and CteTime.scala
  //The attributes of the four "first" links (that is, connected to Source)
  //are the attributes of their Volume Delay Functions and its name
  //the from node is Source and the to node is the respective intermediate node
  //There is also the shape of the link, but it is not yet implemented , so we input empty arrays : new Array[bank.GeoPos](0)

  val FUlink = net.addLink(new Array[bank.GeoPos](0), Source, Unode, BPR, Map("name" -> "FU", "T0" -> 1.0, "α" -> 0.15, "β" -> 4.0, "c" -> 0.2))
  val FUMlink = net.addLink(new Array[bank.GeoPos](0), Source, UMnode, BPR, Map("name" -> "FUM", "T0" -> 2.0, "α" -> 0.15, "β" -> 4.0, "c" -> 0.5))
  val FLMlink = net.addLink(new Array[bank.GeoPos](0), Source, ULnode, BPR, Map("name" -> "FLM", "T0" -> 0.5, "α" -> 0.15, "β" -> 20.0, "c" -> 0.1))
  val FLlink = net.addLink(new Array[bank.GeoPos](0), Source, Lnode, BPR, Map("name" -> "FL", "T0" -> 1.0, "α" -> 0.15, "β" -> 4.0, "c" -> 0.3))

  //for the "second" links, the function does not use any attributes, so we do not need to define them
  //they will take the default values, but we don't really care
  //The only attribute that we define is therefore the name

  val SUlink = net.addLink(new Array[bank.GeoPos](0), Unode, Sink, CteTime, Map("name" -> "SU"))
  val SUMlink = net.addLink(new Array[bank.GeoPos](0), UMnode, Sink, CteTime, Map("name" -> "SUM"))
  val SLMlink = net.addLink(new Array[bank.GeoPos](0), ULnode, Sink, CteTime, Map("name" -> "SLM"))
  val SLlink = net.addLink(new Array[bank.GeoPos](0), Lnode, Sink, CteTime, Map("name" -> "SL"))

  //the next step is to define the matrix
  //we define an Array of Arrays with the format of the Matrix
  val aOa = Array(Array(0.0, 1.0), Array(0.0, 0.0))
  //and we define that Source is the first row (0th position) and Sink is the second row(1st position)
  val rowsnumbers = Map(0 -> Source, 1 -> Sink)
  val mat = new ArrayMatrix(aOa, rowsnumbers)

  //We're now ready to assign
  //The stopping conditions are given by the best relative gap or number of iterations
  //whichever comes first. In our case we use 
  //Best Relative Gap = 0.0001 and number of iterations = 100
  //As it is a simple network we should get to the BRG way before the 100th iteration
  //Every result one wants from the assignment must be explicitly asked
  //The only result implemented now is the volume flow on links given by by analysis.LinksVolumeDurFactory

  val result = net.assign(mat, new bcfw.stoppingCriteria(0.0001, 100), analysis.LinksVolumesDur.apply _)

  //The assignment will print quite a lot of information about objective and convergence
  //We now get the results and print them

  for ((link, flow) <- result) {
    println("the flow on link " + link.atts("name") + " is " + flow)
  }

  //Given the simplicity of the network, the time from Source to Sink 
  //can be calculated with a simple line search  and it is ≈ 2.0002
  //which is not a surprise as the link Upper-Middle has big capacity 
  //and the other three alone would be heavily saturated. If it is to be used, we 
  //would have total time slightly more than its free-flow time 2.0
  //From this we can calculate the analytically the flow on each link that would give 
  //this time, and they are:

  println("The results should be close to ")
  println("Upper " + 0.3213874)
  println("Upper-Middle " + 0.08034284)
  println("Lowe-Middle " + 0.1161594)
  println("Lower " + 0.4820812)

}