package bank
object Attribute {

  /**
   * returns only the attributes that are of Double type, which are the only that can be used in assignment
   */
  def getDoubles[T, Element <: NetworElement](atts: collection.mutable.Map[String, Any]): collection.mutable.Map[String, Double] = {
    atts flatMap { att =>
      att match {
        case (s, d: Double) => List((s, d))
        case _ => Nil
      }
    }
  }
}
case class Attribute[+T, Element <: NetworElement](DefaultValue: T, Name: String)