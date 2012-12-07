package usp.cognitio.msas.agent
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.kernel.MsasObject
import usp.cognitio.msas.simul.log.RichLogAg
import usp.cognitio.msas.Rc

class Ag(val id: Long, var rc: Rc) extends MsasObject {
  def this(id: Long) = this(id, Rc())
  val dotation = rc
  
  var position: Point = Point(0, 0)
  
  def consume(cRc: Rc) : Rc = {
	rc = rc - cRc    
    return rc
  }
  
  override def toString = id.toString
  override def equals(any: Any): Boolean =
    any match {
      case other: Ag => canEquals(any) && id == other.id
      case _ => false
    }
  def canEquals(any: Any): Boolean = any.isInstanceOf[Ag]
  override def hashCode: Int = id.hashCode()
}

object Ag {
  def apply(id: Long, rc: Rc) = new Ag(id, rc)
  def apply(matrix: List[List[Int]]): List[Ag] = {
    var id: Int = 0
    var ags: List[Ag] = Nil
    matrix.foreach(rc => {
      ags = Ag(id, Rc(rc))  :: ags
      id += 1
    })
    return ags.reverse
  }

  def apply(N:Int, mean: Double, variance: Double): List[Ag] = {
    val sigma = Math.sqrt(variance)
    val K = Rc.DIM
    
    val matrix: Array[Array[Int]] = new Array[Array[Int]](N)
    matrix.indices.foreach(i=> matrix(i) = new Array[Int](K))
    
    var randomData = new RandomDataImpl()
    for (k <- 0 to K - 1) {
      randomData.reSeed(System.currentTimeMillis() * (k+1))
      for (i <- 0 to N-1) {
        var distrib = randomData.nextGaussian(mean, sigma).asInstanceOf[Int]
        if (distrib < 0) distrib = distrib * -1

        matrix(i)(k) = distrib
      }
    }

    var listrix : List[List[Int]] = Nil
    matrix.foreach(row => listrix = row.toList :: listrix )
    return apply(listrix.reverse.map(Rc.nonzero(_)))
  }
  
  implicit def AgToLog(ag: Ag): RichLogAg = new RichLogAg(ag)

}
