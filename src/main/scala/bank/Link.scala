package bank
import vdf.VDF
class Link(val id: ID, var shape: Array[GeoPos], val from: Node, val to: Node, var function: VDF, var atts: scala.collection.mutable.Map[String, Any]) extends NetworElement {

}