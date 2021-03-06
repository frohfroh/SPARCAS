package bank
import scala.collection.mutable.HashSet

class Node(val id: ID, var cords: GeoPos, var atts: collection.mutable.Map[String, Any])(implicit network2 : Network) extends NetworElement {
  val arriving: collection.mutable.Set[Link] = new HashSet();
  val exiting: collection.mutable.Set[Link] = new HashSet();
  def network = network2
}