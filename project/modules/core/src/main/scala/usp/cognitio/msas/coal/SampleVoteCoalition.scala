package usp.cognitio.msas.coal
import scala.Math.min
import usp.cognitio.math.Roundable
import usp.cognitio.msas.agent.Ag
import org.apache.commons.math3.analysis.integration.UnivariateIntegrator
import org.apache.commons.math3.analysis.function.Gaussian
import org.apache.commons.math3.analysis.integration.SimpsonIntegrator
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc

class SampleVoteCoalition(private val _mbs: List[Ag], val mean: Double, val variance: Double) extends Coalition(_mbs) with Roundable with Logging {
  val K = Rc.DIM
  val sigma: Double = Math.sqrt(variance)
  def quota(dim: Int) = min(rcPi(dim), rc(dim))

  private val f = new Gaussian(mean, sigma);
  private val integrator = new SimpsonIntegrator();

  def v: Double = 0

  def shapley: List[Double] = {
    var shps: List[Double] = Nil
    mbs.foreach((ag) => {
      var shacum: Double = 0
      for (k <- 0 to K - 1) {
        shacum += shapley(ag, k)
      }
      shps = round(shacum / K) :: shps
    })

    return shps.reverse
  }

  /**
   * Expected Marginal Contribution of Agent
   * ---------------------------------------
   */
  protected def shapley(ag: Ag, dim: Int): Double = {
    val N = mbs.size
    var shacum: Double = 0
    for (X <- 1 to N) {
      shacum += marginal(ag, X, dim)
    }
//    if (isDebugEnabled) debug("Sh Ag " + ag + " " + shacum / N)

    return shacum / N
  }

  /**
   * Expected marginal contribution
   * ------------------------------
   * Expected marginal contribution of agent 'ag' in coalitions of size 'X',
   * in the game represented by dimension 'dim'.
   */
  private def marginal(ag: Ag, X: Int, dim: Int): Double = {
    if (ag.rc(dim) == 0) return 0.00
    val a: Double = (quota(dim).asInstanceOf[Double] - ag.rc(dim)) / X
    val b: Double = (quota(dim) - 0.000001) / X
    return integrator.integrate(10000, f, a, b)
  }

}