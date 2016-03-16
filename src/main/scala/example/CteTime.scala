package example

import vdf.VDF
/**
 * constant time VDF function to serve as an example
 * 
 * In order to see a more interesting example, see BPR
 */
object CteTime extends VDF {
  
    /**
   * No attributes at all
   */
  @Override
  override def attributesNames(): Array[String] = Array()

  /**
   * The time is the constant 1.0
   */
  @Override
  def valueAt(vol: Double, is: Array[Double]): Double = 1.0
  
  /**
   * the derivative of a constant function is 0 
   */
  @Override
  def derivative(vol: Double, is: Array[Double]): Double = 0.0

  /**
   * the integral of a 1 is x
   */
  @Override
  def integral(vol: Double, is: Array[Double]): Double = vol

}