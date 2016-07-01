package bank

abstract class NetworElement extends Ordered[NetworElement] {
  val id: ID;
  def network : Network
  def DoubleAttributes: collection.mutable.Map[String, Double] = Attribute.getDoubles(atts); ;
  var atts: collection.mutable.Map[String, Any];
  override def toString = id.toString()
  def compare(that: NetworElement) = this.id.id.compare(that.id.id)
  def apply(key : String) : Any = atts(key)

}