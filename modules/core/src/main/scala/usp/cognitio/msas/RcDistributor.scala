package usp.cognitio.msas.agent.res
import scala.collection.mutable
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.util.Logging
import usp.cognitio.math.Roundable
import scala.util.Random
import usp.cognitio.msas.Rc

class RcDistributor(val ags:List[Ag], val total:Int, val median : Double, val variance : Double)  
    extends Logging with Roundable {
  val sigma : Double = Math.sqrt(variance)
  
  /** dimensions. */
  val dim = Rc.DIM
  
  var agDistrib : mutable.Map[Ag,List[Int]] = mutable.HashMap.empty[Ag,List[Int]]
  ags.foreach(agDistrib(_) = Nil)
  
  /** Total distribution per dimension. */
  private var dimTotalDistrib = new Array[Int](dim)
  dimTotalDistrib = Array.fill(dim)(0)
  
  /*
   * Do the distribution.
   */
  private var randomData = new RandomDataImpl()
  for (k <- 0 to dim-1) {
    randomData.reSeed()
    for (ag <- ags) {
      var distrib = randomData.nextGaussian(median,sigma).asInstanceOf[Int]
      if (distrib < 0) distrib = distrib * -1
      debug("Distribution for " + ag + ": " + distrib)
      agDistrib(ag) = agDistrib(ag) ::: distrib :: Nil
      
      dimTotalDistrib(k) += distrib
    }
  }
  
  debug("Total distrib: " + dimTotalDistrib.toList)
  
  var agRcs = mutable.HashMap.empty[Ag,List[Int]] 
  /*
   * Transform to resouce quantity
   */
  agDistrib.foreach((el) => {
    val ag = el._1; val distrib : List[Int] = el._2
    var rcs : List[Int] = Nil
    
    for (k <- 0 to dim-1) {
      val q : Int = round(total * (distrib(k).asInstanceOf[Double] / dimTotalDistrib(k))).asInstanceOf[Int]
      rcs = rcs ::: q :: Nil
    }
    agRcs(ag) = rcs
  })

  /* Assign rest. */
  private val random = new Random()
  for (k <- 0 to dim-1) {
    var dif = total - (0 /: agRcs.values.map(_(k))) (_ + _)
    if (dif > 0) {
      val ag = ags(random.nextInt(ags.size))
      val q = agRcs(ag)(k) + dif
      debug("Assigning resto to ag " + ag + " dif: " + dif + " q: " + q)
      agRcs(ag) = agRcs(ag).zipWithIndex.map(el => if (el._2 == k) el._1 + dif else el._1)
    }
  }

}

object RcDistributor {

}