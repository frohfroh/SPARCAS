package bank

//TODO add a apply method that gets its attributes ?
//TODO keep a pointer to the network to which it belongs?
abstract class NetworElement extends Ordered[NetworElement] {
  val id: ID;
  def DoubleAttributes: collection.mutable.Map[String, Double] = Attribute.getDoubles(atts); ;
  var atts: collection.mutable.Map[String, Any];
  override def toString = id.toString()
  def compare(that: NetworElement) = this.id.id.compare(that.id.id)

}