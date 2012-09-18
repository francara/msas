package usp.cognitio.msas.coal
import usp.cognitio.msas.agent.Ag
import usp.cognitio.math.Roundable
import usp.cognitio.msas.util.Logging
import org.apache.commons.math3.analysis.integration.SimpsonIntegrator
import org.apache.commons.math3.analysis.function.Gaussian
import scala.math.min
import org.apache.commons.math3.distribution.NormalDistribution
import usp.cognitio.msas.simul.log.RichLogKSampleCoal
import usp.cognitio.msas.Rc

class KSampleVoteCoalition(private val _mbs: List[Ag]) extends SampleCoalition(_mbs) with Roundable with Logging {
  val K = Rc.DIM
  def quota(dim: Int) = min(rcPi(dim), rc(dim))
//  def quota(dim: Int) = rc(dim)
  def N = members.size

  def v: Double = 0

  def shapley: List[Double] = {
    val N = mbs.size
    var shps: List[Double] = Nil

    if (variance.sum == 0) return for (m <- members) yield (divShp(0))

    mbs.foreach((ag) => {
      var Esh = 0.00
      for (X <- 1 to N) {
        Esh += kEsh(ag, X)
        debug("Esh ag" + ag + " X " + X + " = " + Esh)
      }
      shps = round(Esh / N) :: shps
    })

    return norm(shps.reverse)
  }

  protected def kEsh(ag: Ag, X: Int): Double = {
    var kEsh = 0.00
    for (k <- 0 to K - 1)
      if (variance(k) == 0) kEsh += 1/(N*N)
      else {
        var prod = 1.00
        for (f <- 0 to k - 1) {
          prod = prod * PL(ag, X, f)
        }

        for (g <- k to K - 1) {
          prod = prod * PW(ag, X, g)
        }

        debug("kEsh ag " + ag + " X " + X + " k " + k + " = " + prod)
        kEsh += prod
      }

    return kEsh
  }

  private def dummy(ag: Ag, k: Int) = ag.rc(k) == 0

  private def divShp(k: Int): Double = {
    return round(1.00 / members.size)
  }

  protected def PL(ag: Ag, X: Int, k:Int): Double = {
    val k_mean = X * mean(k)
    val k_sigma = Math.sqrt(variance(k)/X)

    val a: Double = (quota(k).asInstanceOf[Double] - ag.rc(k))
    val b: Double = (quota(k) - 0.000001)

    if (b < a) {
      debug("incongruencia")
    }
    val f = new NormalDistribution(k_mean, k_sigma)

    return f.cumulativeProbability(a, b)
  }

  protected def PW(ag: Ag, X: Int, k:Int): Double = {
    val k_mean = X * mean(k)
    val k_sigma = Math.sqrt(variance(k)/X)

    val a: Double = quota(k)
    val b1: Double = quota(k) - ag.rc(k) - 1
    /* ***   infinite   *** */
    val b2: Double = 1000 * quota(k)

    val f = new NormalDistribution(k_mean, k_sigma)

    if (b1 > 0)  return f.cumulativeProbability(0, b1) + f.cumulativeProbability(a, b2)
    else return f.cumulativeProbability(a,b2)
  }

  protected def norm(values : List[Double]) = values.map(v => round(v.asInstanceOf[Double] / values.sum))

  implicit def CoalToLog(coal:KSampleVoteCoalition) : RichLogKSampleCoal = new RichLogKSampleCoal(coal)

}
