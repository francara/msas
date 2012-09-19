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

case class EgoCog(_ag:MsasAg) extends Ego(_ag) with PlanProspector with PlanMapper {
  Point.MAX = 10
  val space = Space()

  def rc = ag.rc
  def rcPi = ag.rcPi
  def target : Point = (2,2)
  
  def init(wphy: WorldPhy, wsoc: WorldSoc) {
    
  }
  
  def act(sense:WorldSense) : Plan = {
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

    /*
     * Transforms plans into required resources.
     */
    val pi = this.mapit(sense, plan)
    
    /*
     * If there is a lack os resources we should try to coligate.
     */
    if (pi.sum > 0)  plan.addFirst(ActSoc())
    
    return plan
  }
    
}