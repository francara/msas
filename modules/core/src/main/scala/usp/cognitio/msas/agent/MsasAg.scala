package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.EgoCog
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.env.WorldSoc
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.agent.cog.NullPlan
import usp.cognitio.msas.agent.cog.SingletonPlan
import usp.cognitio.msas.coal.Coalition

class MsasAg(_id: Long, _rc: Rc) extends Ag(_id, _rc) with Player {
  var ecog: EgoCog = null
  var esoc: EgoSoc = null
  var body: Body = null

  var plan: Plan = NullPlan()

  var rcPi: Rc = Rc()
  
  def u : Double = esoc.u
  def u(al: Rc): Double = esoc.u(al)

  def init(wphy: WorldPhy, wsoc: WorldSoc) {
    body = Body(this, wphy, wsoc)
    ecog = EgoCog(this)
    esoc = EgoSoc(this)
  }

  def act(sense: WorldSense) {
    /*
     * An agent should generate a new plan
     * only if needed.
     */
    plan match {
      case NullPlan() => plan = ecog.act(sense)
      case p : SingletonPlan => p.next 
      case _ =>
        val nplan = ecog.act(sense)
        merge(nplan, plan)
    }

    /*
     * The environment may have changed.
     */
    val nrcPi = ecog.mapit(sense, plan)
    if (nrcPi != rcPi) {
      rcPi = nrcPi
      esoc.act(sense, plan)
    }

    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }

  def merge(plan1: Plan, plan2: Plan): Plan = {
    return plan1
  }

  def coalition: Coalition = esoc.coalition
  def consume(rcCoal: Rc, q: Int): Rc = esoc.consume(rcCoal, q)
  
}