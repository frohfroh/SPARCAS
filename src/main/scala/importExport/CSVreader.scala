package importExport
import bank.Network
import collection.mutable
import vdf.VDF
import bank.Attribute
import bank.Node
import bank.Link
import bank.ArrayMatrix
/**
 * imports (reads) a network and/or matrices from csv files 
 */
object CSVreader {
  /***
   * creates a Network from csv files. The csv files must use the separator as specified by the parameter sep.
   * They must have readers with attributes names. The following attributes are mandatory
   * for nodes:
   * 	an ID, to be used for importing, different of the one that will be later assigned to it.
   *  an indication if it is a centroid
   * for links:
   *  the ID of the from node
   *  the ID of the to node
   *  the volume delay function
   * @param links_file csv file with one line for every link , the columns should contain the attributes
   * @param nodes_file csv file with one line for every node , the columns should contain the attributes
   * @param nodeId the name of the attribute of the node csv file to be used as a temporary ID, must match the id used on the links
   * @param fromNode the name of the attribute of the link csv file with the node from id
   * @param toNode the name of the attribute of the link csv file with the node to id
   * @param vdf the name of the attribute of the link csv file with the position of its vdf on the vdfs array
   * @param centroid the name of the attribute of the node csv file that should be
   * 					1 if it is a centroid
   * 					0 otherwise
   * @param sep the separator used for the csv files. No escaping, quoting  , commenting or missing values is allowed
   * @param vdfs an array with the VDF functions used by this network. 
   */
  def readNetwork(links_file: scala.io.Source, nodes_file: scala.io.Source , nodeId : String , fromNode : String , 
      toNode : String , vdf : String , centroid : String , sep : String ,  vdfs: Array[VDF]) : Network = {
     val net: Network = new Network
     
       def processAttributes[T <: bank.NetworElement](file : scala.io.Source, crt : Attribute[Any, T] => Unit ) : List[(String, List[Any])] = {
       //val AttsUntyped = readCSV(file , sep)
       //val Atts = AttsUntyped.map({case (name , values) => (name,defineType(values) )})
       val Atts2 = readCSVtyped(file , sep)
       for( (name , vals) <- Atts2){
       vals(0) match { //We use the first element because of type erasure, not sure if there is any better way  
         case _ : Int => crt(Attribute[Int, T](0, name))
         case _ : Double => crt(Attribute[Double, T](0, name))
         case _ : Long => crt(Attribute[Long, T](0, name))
         case _ => crt(Attribute[String, T]("", name))
       }
     }
      Atts2.map({case (name , values) => (name,values.toList )}).toList
     }
     
     val ndsAtts2 = processAttributes(nodes_file , {net.addNodeAttribute(_ : Attribute[Any, Node])} )
     val lksAtts2 = processAttributes(links_file , {net.addLinkAttribute(_ : Attribute[Any, Link])} )
     
     val headerNodes = ndsAtts2.map(_._1)
     val ndsAttsT = ndsAtts2.map(_._2).transpose.map{headerNodes.zip(_).toMap}  //each element is the attributes map os a node
     val headerLinks = lksAtts2.map(_._1)
      val lksAttsT = lksAtts2.map(_._2).transpose.map{headerLinks.zip(_).toMap}  //each element is the attributes map os a node

     val nodes = (for( atts  <- ndsAttsT) yield {
       val isCentroid = atts(centroid)
       val node = isCentroid match {
         case 1 => net.addCentroid(null, mutable.Map() ++ atts)
         case 0 => net.addNode(null, mutable.Map() ++ atts)
         case v => throw new IllegalArgumentException("Centroid values must be necessarilly 0 for regular nodes or 1 for centroids. "+
            v+" found on node whose id is "+atts(nodeId) )
       }
       (atts(nodeId) , node)
     }).toMap
     
     for( atts  <- lksAttsT)  {
       val vdf2 = vdfs({atts(vdf) match {
         case x : Int => x
         case err => throw new IllegalArgumentException("vdf must be integers . "+
            err+" found on link whose nodes ids are "+atts(fromNode)+" - "+atts(toNode ))
       }})
       val ndf = nodes(atts(fromNode))
       val ndt = nodes(atts(toNode))
       net.addLink(new Array[bank.GeoPos](0), ndf, ndt, vdf2, mutable.Map() ++ atts)
     }
    net
  }

  def readCSV(source: scala.io.Source , sep : String) : Array[(String, Array[String])]  = {
    val splitter = mySplit( _ : String , sep)
    val lines = source.getLines()
    val header = lines.next()
    val attributes = splitter(header)
    val linesArr = lines.toArray.map(splitter(_))
    //val mat =  Array.ofDim[String](linesArr.length, attributes.length)
    val matt = Array.tabulate[String]( attributes.length,linesArr.length){(i,j) => linesArr(j)(i)}
    attributes zip  matt
  }
  def readCSVtyped(source: scala.io.Source , sep : String) : Array[_ <: (String, Array[_])]  = {
    val splitter = mySplit( _ : String , sep)
    val lines = source.getLines()
    val header = lines.next()
    val attributes = splitter(header)
    val linesArr = lines.toArray.map(splitter(_))
    //val mat =  Array.ofDim[String](linesArr.length, attributes.length)
    val matt = Array.tabulate[String]( attributes.length,linesArr.length){(i,j) => linesArr(j)(i)}
    val untyped = attributes zip  matt
    untyped.map({case (name , values) => (name,defineType(values) )})
  }
  def mySplit(line : String , sep : String) : Array[String] =  line.split(sep) filter (_ != "")

  def defineType(arr : Array[String]) : Array[_ <: Any] = {
    def trier(funcs : List[String => Any]) : Array[_ <: Any] = funcs match {
        case Nil => arr
        case f :: tail => {
          try{
            arr.map { f(_) }
          } catch {
            case _: Throwable =>trier(tail)  
          }
        }
      }
    def trier2[T](funcs : List[String => Any]) : Array[_ <: Any] = funcs match {
        case Nil => arr
        case f :: tail => {
          try{
            arr.map { f(_) }
          } catch {
            case _: Throwable =>trier(tail)  
          }
        }
      }
    trier( {x: String => x.toInt} :: {x: String => x.toLong} :: {x: String => x.toDouble} :: Nil)    
	}
  
  /**
   * reads a matrix from a csv file
   * 
   * A network must be provided so that one can relate every origin/destination with a centroid.
   * Cells not defined are 0.0.
   * 
   * @param net the Network containing the centroids for this matrix
   * @param file the CSV file faving a column for the origin node, a column for the destination node and a column for the value
   * @param att the name of the node attribute that will be used to identify the centroid
   * @param from the name of the column with the value of the att attribute on the origin centroid
   * @param to the name of the column with the value of the att attribute on the destination centroid
   * @param value the value (must be a double) of the cell
   * @param sep the separator character used (as a String)
   */
  def readMatrix(net : Network , file: scala.io.Source,  att : String , from : String , to : String , value : String , sep : String) : ArrayMatrix = {
    val columns : Array[_ <: (String, Array[_])] = readCSVtyped(file , sep)
    val mapa = columns.map({case (name , values) => (name,values.toList )}).toList.toMap
    val nodde_att = net.nodes.values.map { x => (x.atts(att) , x) }.toMap
    val cents = net.centroides.values.toList
    val node2pos = cents.zipWithIndex.toMap
    val origin = mapa(from).map {x=>  node2pos(nodde_att(x)) }
    val destination = mapa(to).map { x=>  node2pos(nodde_att(x)) }
    val triples = (mapa(value) , origin , destination).zipped.toList
    val aOa  = Array.fill(net.centroides.size, net.centroides.size)(0.0)
    for( (v , f , t) <- triples){
        val str = v.toString()
        try{
          val dv = str.toDouble
          aOa(f)(t) = dv
        }catch {
          case _ : java.lang.NumberFormatException => throw new java.lang.NumberFormatException("The values of the matrix must be float but "+str+" is not")
        }
    }
    new ArrayMatrix(aOa , node2pos map(_.swap) )
  }
  
}