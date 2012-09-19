package usp.cognitio.msas.agent.soc
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.math.Roundable
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.soc.Socialization.context
import usp.cognitio.msas.agent.soc.Aval
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.soc.SocializationContext
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.simul.compartilhamento.SimulConfiguration
import usp.cognitio.msas.simul.log.trace
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Player

/**
 * Socialization abilities of an agent.
 */
trait Socialization extends Ag with Roundable with Logging {
  /*
   * Abstract attributes.
   * Defined in Ag.
   */
  var rcPi: Rc
  var rc: Rc
  var coalition: Coalition = context.createCoalition(this)

  /**
   * Current utility.
   * --
   *
   */
  def u: Double = if (rcPi.sum > 0) round(consume(rc, rcPi.sum).sum.asInstanceOf[Double] / rcPi.sum) else 1.0
  def u(al: Rc): Double = if (rcPi.sum > 0) round(consume(al, rcPi.sum).sum.asInstanceOf[Double] / rcPi.sum) else 1.0

  /** Avaliates a coligation with 'who' and returns the future utility. */
  def avaliation(who: Ag): Double = {
    coalition.add(who)
    var aloc = coalition.alocate(this)
    coalition.remove(who)
    return u(aloc)
  }


  def avaliate(who: Ag): Aval = {
    val isMember: Boolean = if (coalition.members.contains(who)) true else false

    val beforeWellfare = coalition.u

    trace aval (this, who)
    if (!isMember) coalition.add(who)
    var alocs = coalition.alocate()
    val afterWellfare = coalition.u(alocs)
    if (!isMember) coalition.remove(who)
    if (!alocs.contains(this) || !alocs.contains(who)) {
      error("Ag not found in coalition: this: " + this + ", who: " + who)
    }
    //    return (this.u(alocs(this)), who.u(alocs(who)), afterWellfare)
    return new Aval(who, this, alocs, afterWellfare)
  }

  /** Avaliates the resource usage. Which resources I would use. */
  def consume(rcCoal: Rc, q: Int): Rc = {
    /** Consume max 'q' of 'index' from resource 'rc'. */
    def eat(rc: Rc, index: Int, q: Int): Rc = {
      if (q == 0) rc
      else if (index == Rc.DIM) rc
      else {
        var maxq = Math.min(rcPi(index), q)
        var nrc = new Rc(
          rc.toList.zip(rc.toList.indices)
            .map(el => if (el._2 == index) Math.max(0, el._1 - maxq) else el._1))
        eat(nrc, index + 1, q - (rc(index) - nrc(index)))
      }
    }

    var rcEaten = eat(rcCoal, 0, q)
    return rcCoal - rcEaten
  }

  /** Effectivate a coligation. */
  def coligate(who: Ag): Coalition = {
    if (coalition.members.contains(who)) return coalition

    coalition.add(who)
    who.coalition.remove(who)
    who.coalition = this.coalition

    this.coalition.distribute()

    return this.coalition
  }

  def abandon() {
    if (SimulConfiguration.REDISTRIBUTE) this.coalition.restore()
    this.coalition.remove(this)
    if (SimulConfiguration.REDISTRIBUTE) this.coalition.distribute()

    this.coalition = context.createCoalition(this)
  }

}

object Socialization {
  var context: SocializationContext = null
  def apply(f: (List[Ag]) => Coalition): Unit = { context = new SocializationContext(f) }
  def apply(ags: List[Ag], mean: Double, variance: Double): List[Socialization] = {
    val sigma = Math.sqrt(variance)
    val N = ags.size; val K = Rc.DIM
    var social: List[Socialization] = (for (i <- 0 to N - 1) yield new Ag(ags(i).id, ags(i).rc) with Socialization {
      var rcPi = Rc()
    }).toList

    var randomData = new RandomDataImpl()
    for (k <- 0 to K - 1) {
      randomData.reSeed(System.currentTimeMillis() * k * k)
      for (i <- 0 to N - 1) {
        val ag = social(i)
        var distrib = randomData.nextGaussian(mean, sigma).asInstanceOf[Int]
        if (distrib < 0) distrib = distrib * -1
        else if (distrib == 0) distrib = 1
        ag.rcPi = Rc(ag.rcPi, k, distrib)
      }
    }

    return social
  }

  implicit def agToSocialization(ag: Ag): Socialization = { ag.asInstanceOf[Socialization] }
}
