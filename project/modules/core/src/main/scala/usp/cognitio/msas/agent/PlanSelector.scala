package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.env.WorldPhy

trait PlanSelector {
  def select(phy: WorldPhy, plans: List[Plan]) : Plan
}