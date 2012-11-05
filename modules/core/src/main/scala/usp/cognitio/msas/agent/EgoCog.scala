package usp.cognitio.msas.agent
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.plan.AStarPlanner
import usp.cognitio.math.alg.Point
import usp.cognitio.math.alg.Point._
import usp.cognitio.msas.agent.cog.plan.Space
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.env.WorldSoc
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.agent.cog.SingletonPlan

case class EgoCog(_ag:MsasAg) extends Ego(_ag) with PlanProspector with PlanMapper {  
  val _space = Space()
  
  def space = _space
  def rc = ag.rc
  def rcPi = ag.rcPi
  def target : Point = ag.target
  
  def think(sense:WorldSense) : Plan = {
    /*
     * Build plans according the state space.
     * 
     * PHYSICAL ACTS
     * -------------
     * The plan initially contains only physical acts.
     * 
     * SOCIAL ACTS
     * -----------
     * Social acts are a result of the lack of resources,
     * hence firstly we transform a plan into requierd resources.
     */
    val plans = this.build(sense)
    val plan = plans(0)
    if (plan.isNull) return plan
    /* Step out the current position. */
    plan.next

    /*
     * Transforms plans into required resources.
     */
    ag.rcPi = this.mapit(sense, plan)
    
    /*
     * If there is a lack os resources we should try to coligate.
     */
    if ((rc ^- ag.rcPi).sum > 0)  plan.addFirst(ActSoc())
    
    return SingletonPlan(plan)
  }
    
}