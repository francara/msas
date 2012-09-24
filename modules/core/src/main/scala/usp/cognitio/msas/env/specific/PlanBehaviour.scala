package usp.cognitio.msas.env.specific
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.EgoCog
import usp.cognitio.msas.agent.EgoSoc
import usp.cognitio.msas.agent.cog.NullPlan
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.Body
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.SingletonPlan
import usp.cognitio.msas.agent.ActPhy

trait PlanBehaviour {
  var ecog: EgoCog
  var esoc: EgoSoc
  var body: Body

  var plan: Plan

  var rc : Rc
  var rcPi: Rc
  
  var target: Point
  
  def act(sense: WorldSense) {
    throw new UnsupportedOperationException()
  }
}

trait PlanOnceActAllBehaviour extends PlanBehaviour {
  override def act(sense: WorldSense) {
    if (plan.finished) plan = NullPlan()
    /*
     * An agent should generate a new plan
     * only if needed.
     */
    plan match {
      case NullPlan() => plan = ecog.think(sense)
      case p : SingletonPlan => true 
      case _ =>
        val nplan = ecog.think(sense)
        merge(nplan, plan)
    }

    /* 
     * The think process can generate a NullPlan
     * if the target has already been reached.
     */
    if (plan.isNull) return
    
    if (plan.action.isPhy) {
      body.act(plan.action.asInstanceOf[ActPhy])
      plan.next
      return
    }
    
    /*
     * The environment may have changed.
     */
    val nrcPi = ecog.mapit(sense, plan)
    if (nrcPi != rcPi) {
      rcPi = nrcPi
      if (plan.action.isSoc) esoc.act(sense, plan)
      plan.next      
    }

    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }
  
  def merge(plan1: Plan, plan2: Plan): Plan = {
    return plan1
  }
}
trait PlanActBehaviour