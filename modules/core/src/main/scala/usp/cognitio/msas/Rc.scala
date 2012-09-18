/*
 * Resources class.
 * 
 * Date: 30/04/2012
 * Author: Frank Cara
 */
package usp.cognitio.msas
import scala.util.Random

/**
 * Vector of resources.
 */
class Rc(val resources: List[Int]) {


  def apply(i: Int): Int = resources(i)

  def +(other: Rc): Rc = new Rc(resources.zip(other.resources).map(el => el._1 + el._2))
  def -(other: Rc): Rc = new Rc(resources.zip(other.resources).map(el => el._1 - el._2))
  
  /**
   * Calculates the free resource vector from a planning vector.
   */
  def ^+(other: Rc): Rc =
    new Rc(
      resources.zip(other.resources)          // zip[ (1,2,3), (1,1,1)] => ((1,1),(2,1),(3,1))
        .map(el => el._1 - el._2)             // ((1,1),(2,1),(3,1)) => (0,1,2)
        .map(el => if (el > 0) el else 0)     // removes the negatives
        )

  /**
   * Calculates the resource vector from a planning vector.
   */
  def ^-(other: Rc): Rc =
    new Rc(
      resources.zip(other.resources)            // zip[ (1,2,3), (1,1,1)] => ((1,1),(2,1),(3,1))
        .map(el => el._1 - el._2)               // ((1,1),(2,1),(3,1)) => (0,1,2)
        .map(el => if (el < 0) -1 * el else 0)  // removes the positives
        )

  def sum : Int = (0 /: resources)(_ + _)
    
  def length = resources.length
  def toList = resources
  override def toString = resources.mkString("[", ",", "]")
  override def equals(any : Any) : Boolean =
    any match {
    case other : Rc => canEquals(any) && resources == other.resources
    case _ => false
  }
  def canEquals(any : Any) : Boolean = any.isInstanceOf[Rc]
  override def hashCode : Int = resources.hashCode() 

}

object Rc {
  var DIM = 3
  
  /** List constructor: ex: Rc(1,2,3) */
  def apply(els: Int*): Rc = if (els.length == 0) Rc.nil else new Rc(els.toList)
  def apply(rcs: List[Int]) = new Rc(rcs)
  def apply(rc:Rc, k:Int, vl:Int) = {
    val rcs : List[Int] = (for (kk <- 0 to DIM-1) yield if (kk == k) vl else rc(kk)).toList
    new Rc(rcs)
  }
  
  def nil : Rc = zero(DIM)
  def zero(dim : Int) : Rc = new Rc(List.make(dim, 0))
  def zero() : Rc = zero(DIM)
  
  def sum(rcs : List[Rc]) : Int = (0 /: rcs.map(rc => rc.sum))(_ + _)
  def sum(rcs : List[Rc], dim : Int) : Int = (0 /: rcs.map(rc => rc(dim)))(_ + _)

//  def :?(rc:Rc) = if (rc == null) Rc.nil else rc

  def distribute(total : Int) : Rc = {
    if (total/DIM < DIM) throw new UnsupportedOperationException("Minimum resource qtd " + DIM*DIM)
    var rcs : List[Int] = Nil
    var rest: Int = total
    val random = new Random()
    for (k <- 0 to DIM-2) {
      val rd = random.nextInt(rest/DIM) + 1
      rcs = rd :: rcs
      rest -= rd
    }
    rcs = rest :: rcs
    Rc(rcs)
  }
  
  def total(rcs : List[Rc]) : Rc = {
    var nrc : Rc = Rc.zero(rcs(0).length)
    rcs.foreach( rc => {
      nrc = nrc + rc
    })
    return nrc
  }

  def nonzero(rcs : List[Int]) : List[Int] = rcs

  /**
   * -----------------------------
   * ---   Null test dialect   --- 
   */
  class :?(val rc : Rc)
  object :? { def apply(rc:Rc) = new :?(rc)}
  
  implicit def CommaCommaQuesttoRc(test: :?) = if (test.rc == null) Rc.nil else test.rc
  
}