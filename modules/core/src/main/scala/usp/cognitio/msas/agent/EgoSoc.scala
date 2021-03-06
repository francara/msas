package usp.cognitio.msas.agent
import usp.cognitio.math.Roundable
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.agent.MindTraceable
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.Rc

case class EgoSoc(_ag: MsasAg) extends Ego(_ag) with Roundable with MindTraceable {
  case class Candidate(val neigh: Ag, val u: Double, val session: SessionSoc)
  var coalition: Coalition = ag.body.soc.createCoalition(ag)
  
  def rc: Rc = ag.rc
  def rcPi: Rc = ag.rcPi

  def u: Double = uPerK(rc)
  def u(al: Rc): Double = uPerK(al) 

  private def uPerAll(theRc: Rc): Double = if (rcPi.sum > 0) round(consume(theRc, rcPi.sum).sum.asInstanceOf[Double] / rcPi.sum) else 1.0
  private def uPerK(theRc: Rc) : Double = 
    (
        for (k <- 0 to Rc.DIM-1) 
        yield if (rcPi(k) > 0) Math.min(theRc(k), rcPi(k)) / (rcPi(k).asInstanceOf[Double] * Rc.DIM ) else 1D/Rc.DIM 
    ).sum
  
  def act(sense: WorldSense, plan: Plan): Boolean = {
    debug(sense, "Soc", "U:" + u)
    def pre(ag: Ag) : String = "[NEIGH:" + ag.toString() + "]"
    val action = plan.action
    if (action.isPhy) return false

    var candidates: List[Candidate] = Nil
    /*
     * If there is a social action, avaliate
     * coalitions.
     */
    sense.neighs.filter(!coalition.members.contains(_))
      .foreach(neigh => {
        val session = ag.body.soc.communicate(ag, neigh)
        val alloc = session.allocate()
        trace(sense, "Avaliate", pre(neigh) + " U:" + u(alloc) + " DoubleWin:" + session.doubleWin())
        ag.qtdAval += 1
        if (u(alloc) > u && session.doubleWin()) {
          candidates = Candidate(neigh, u(alloc), session) :: candidates
        }
      })

    /*
     * !!! GUARD CONDITION   !!!
     */
    if (candidates.size == 0) return false

    /*
     * Associate with the best coalition.
     */
    ag.qtdColigate += 1
    val candidate = candidates.sort(_.u > _.u).head
    info(sense, "Coligate", pre(candidate.neigh))
    if (candidate.session.coligate()) {
      this.coalition = candidate.session.coalition
      return true
    } else {
      return false
    }

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
  
  /**
   * Returns a resource with the amount consumed from rc.
   */
  def consume(rc: Rc, q:Int, k: Int) : Rc = {
    val r_q = Math.min(rc(k), q)
    Rc(rc, k, r_q)
  }
  
  
}