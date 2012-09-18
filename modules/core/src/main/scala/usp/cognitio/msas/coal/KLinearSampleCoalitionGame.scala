package usp.cognitio.msas.coal
import org.apache.commons.math3.distribution.NormalDistribution
import usp.cognitio.msas.agent.Ag
import usp.cognitio.math.Roundable
import usp.cognitio.msas.simul.log.RichLogKSampleCoal
import usp.cognitio.msas.util.Logging
import scala.math.min
import usp.cognitio.msas.simul.log.RichLogKLinearSampleCoal
import usp.cognitio.msas.simul.log.RichLogString
import usp.cognitio.msas.Rc

class KLinearSampleCoalitionGame (private val _mbs: List[Ag]) extends SampleCoalition(_mbs) with Roundable with Logging {
  val K = Rc.DIM
  def quota(dim: Int) = min(rcPi(dim), rc(dim))
  def N = members.size

  def v: Double = 0

  def shapley: List[Double] = {
    if (N < 6) {
      val p_coal = new VoteCoalition(members)
      return p_coal.shapley
    }  
    
    var shps: List[Double] = Nil

    if (variance.sum == 0) return for (m <- members) yield (divShp(0))

    mbs.foreach((ag) => {
      var Esh = 0.00
      for (X <- 1 to N) {
        Esh += kEsh(ag, X)
        trac("Esh ag" + ag + " X " + X + " = " + Esh)
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
          prod = prod * (1 - PL(ag, X, f))
        }

        prod = prod * PL(ag, X, k)
        
        for (g <- k+1 to K - 1) {
          prod = prod * PW(ag, X, g)
        }

        trac("kEsh ag " + ag + " X " + X + " k " + k + " = " + prod)
        kEsh += prod
      }

    return kEsh
  }

  private def divShp(k: Int): Double = {
    return round(1.00 / members.size)
  }

  protected def PL(ag: Ag, X: Int, k:Int): Double = {
    val k_mean = X * mean(k)
    val k_sigma =  
      if (variance(k) > 0) Math.sqrt(variance(k)/X)
      else 0.1E-20

    val a: Double = (quota(k).asInstanceOf[Double] - ag.rc(k))/X
    val b: Double = (quota(k) - 0.1E-40)/X

    val f = new NormalDistribution(mean(k), k_sigma)

    if (a<b) return f.cumulativeProbability(a, b)
    else return 0.1E-40
  }

  protected def PW(ag: Ag, X: Int, k:Int): Double = {
    val k_mean = X * mean(k)
    val k_sigma =  
      if (variance(k) > 0) Math.sqrt(variance(k)/X)
      else 0.1E-20
    val a: Double = (quota(k) - ag.rc(k))/X
    /* ***   infinite   *** */
    val b: Double = 1000 * quota(k)

    val f = new NormalDistribution(mean(k), k_sigma)

    if (a<b) return f.cumulativeProbability(a,b)
    else return 0.1E-40
  }

  protected def norm(values : List[Double]) =
    values.map(v => {
      val sum = values.sum
      if (sum > 0.00) round((v.asInstanceOf[Double] / values.sum))
      else v
    })

  implicit def CoalToLog(coal:KLinearSampleCoalitionGame) : RichLogKLinearSampleCoal = new RichLogKLinearSampleCoal(coal)
  implicit def StringToLog(msg: String): RichLogString = new RichLogString(msg)

}
