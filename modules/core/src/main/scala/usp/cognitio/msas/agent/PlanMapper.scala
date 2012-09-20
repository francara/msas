package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.cog.plan.Space
import usp.cognitio.msas.env.WorldSense

trait PlanMapper {
  def space : Space
  def rc : Rc
  
  def mapit(sense:WorldSense, plan: Plan) : Rc = {
    val moves = plan.acts.filter(act => act.isInstanceOf[ActPhy]).map(act => act.asInstanceOf[ActPhy].target)
    val rcs : List[Rc]= moves.map(pt => sense.rcs(pt.x)(pt.y))
    return Rc.total(rcs)
  }
}