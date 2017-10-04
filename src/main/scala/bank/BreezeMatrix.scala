package bank
import breeze.{linalg => bz}
import scala.reflect.ClassTag
import breeze.storage.Zero

//TODO some covariance with B type, so that one can operate on generic BreezeMatrix s

//we redefine shapeless <:!< here
//we could depend on shapeless, but I did not want for a such small need
//kind of lame because we depend on breeze that depend on shapeless anyway
trait <:!<[A, B]

/**
 * this class is the new standard of Matrix in SPARCAS
 * it uses a breeze matrix, which is flexible enough for both dense and sparse matrix
 */
class BreezeMatrix[T: ClassTag , B <: bz.Matrix[T] with bz.EquallyCopiable[B]]
        (val bm : B,val mapa: scala.collection.Map[Int, Node])(implicit val toDenseMatrix : B => bz.DenseMatrix[T])
        extends Matrix[T] {
  import BreezeMatrix._
  def toArrays(mapa2: Map[Node, Int]): Array[Array[T]] ={
    val am = toBreeze(mapa2)
    toAoA(toDenseMatrix(am))
  }
  def toBreeze(implicit mapa2: Map[Node, Int]) : B = {
    if(mapa2 == mapa) bm
    else{
      val am : B= bm.copy
      for(i <- 0 until bm.rows ; j <- 0 until bm.cols){
       am(mapa2(mapa(i)), mapa2(mapa(j))) = bm(i,j)
      }
      am
    }
  }
  def value(par: (Node, Node)): T = {
    val volta = mapa.map(_.swap)
    bm(volta(par._1) ,volta( par._2))
  }

 
}

object BreezeMatrix{
   def toAoA[T : ClassTag](dm : bz.DenseMatrix[T]) : Array[Array[T]] =  Array.tabulate[T](dm.rows,dm.cols) { dm(_,_) }
   def fromAoA[T : ClassTag](aoa : Array[Array[T]]) : bz.DenseMatrix[T] = {
     new bz.DenseMatrix(aoa.length,aoa(0).length,aoa.flatten).t
   }
   def apply(am : ArrayMatrix) : BreezeMatrix[Double , bz.DenseMatrix[Double]] = {
     new BreezeMatrix[Double , bz.DenseMatrix[Double]](fromAoA(am.raw) , am.mapa)
   }
   
  implicit def nsub[A, B] : A <:!< B = new <:!<[A, B] {}
  implicit def nsubAmbig1[A, B >: A] : A <:!< B = sys.error("Unexpected call")
  implicit def nsubAmbig2[A, B >: A] : A <:!< B = sys.error("Unexpected call")
  
  
  //There are two ways a matrix can be "converted" to a densematrix
  //one is if it is already a denseMatrix
  //the other one is if its parametric type has a Zero
  //the scala's predef conforms gives us the first way
  //and the following function the second one
  //however the two would clash and we would have an ambiguity for DenseMatrix[T : Zero]
  //which would be very painful because DenseMatrix[Double] is by far the most common case
  //so we use the "is not subtype" operator to make ZeroTagged not applicable for DenseMatrix
  implicit def ZeroTagged[T: ClassTag : Zero , B <: bz.Matrix[T]](implicit ev :  B <:!< bz.DenseMatrix[T]) : (B => bz.DenseMatrix[T])= _.toDenseMatrix
   //implicit def ZTDintersection[T: ClassTag : Zero] : (bz.DenseMatrix[T] => bz.DenseMatrix[T]) = x => x
   //implicit def alreadyDense[T] : (bz.DenseMatrix[T] => bz.DenseMatrix[T]) = x => x

}