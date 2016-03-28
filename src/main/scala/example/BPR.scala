package example

import vdf.VDF
/**
 * simple BPR function to serve as an example
 *
 * Every Volume Delay Function used must be defined as a class/object extending the VDF abstract class
 * One must provide implementation for Integral, Derivative and the function itself
 *
 * The function should give the time to go through a link given the volume on that link
 * As input it receives the volume and the link attributes that it declares as needed
 *
 * In order to declare attributes as needed , it returns an Array with the attribute names
 * At assignment time, these attributes must be defined on the network
 * Only attributes of type Double can be used
 * It receive the attribute values of a link as a Array[Double] in which each value is one attribute
 * in the order declared in  attributesNames()
 *
 * The derivation and integration are to be calculated in respect to the volume
 */
object BPR extends VDF {

  /**
   * here we give the attributes that we need. We will receive them in the order we declare them here
   *
   */
  @Override
  override def attributesNames(): Array[String] = Array("α", "β", "c", "T0")

  /**
   * This is a optional helper function
   * we basically take the array and give to each of its elements the name of the attribute
   * Then we calculate the derivative, function or integral as demanded
   * This is done so that the naming of each array element is done only once
   */
  def PID(vol: Double, is: Array[Double], func: String): Double = {
    val α: Double = is(0)
    val β: Double = is(1)
    val c: Double = is(2)
    val T0: Double = is(3)
    func match {
      case "value" => T0 * (1 + α * Math.pow(vol / c, β))
      case "integral" => T0 * vol + α * T0 * Math.pow(vol / c, β + 1.0) * c / (β + 1.0);
      case "derivative" => α * β * T0 * Math.pow(vol / c, β - 1.0) / c
    }
  }
  @Override
  def valueAt(vol: Double, is: Array[Double]): Double = {
    PID(vol, is, "value")
  }

  @Override
  def derivative(vol: Double, is: Array[Double]): Double = {
    PID(vol, is, "derivative")
  }

  @Override
  def integral(vol: Double, is: Array[Double]): Double = {
    PID(vol, is, "integral")
  }

}

