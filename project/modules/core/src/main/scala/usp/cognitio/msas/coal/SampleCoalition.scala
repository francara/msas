package usp.cognitio.msas.coal
import usp.cognitio.msas.agent.Ag
import usp.cognitio.math.Roundable
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc

abstract class SampleCoalition(private val _mbs: List[Ag]) extends Coalition(_mbs) with Roundable with Logging {

  def mean : List[Double] = {
    var m : List[Double] = Nil
    for (k <- 0 to Rc.DIM-1) m = members.map(_.rc(k)).sum.asInstanceOf[Double] / members.size :: m
    return m.reverse
  }

  def variance : List[Double] = {
    val mi = mean
    var varia : List[Double] = Nil
    for (k <- 0 to Rc.DIM-1) {
      val N = members.size
      varia = members.map(ag => (ag.rc(k) - mi(k))*(ag.rc(k) - mi(k)).asInstanceOf[Double]/N)
        .sum :: varia
    }
    return varia.reverse
  }
  
}