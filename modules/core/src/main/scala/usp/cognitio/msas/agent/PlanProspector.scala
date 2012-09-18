package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.agent.cog.Plan._
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.agent.cog.dp.DPPlanner
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.plan.AStarPlanner
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.cog.plan.Space

trait PlanProspector {

  val space : Space
  def target : Point

  /**
   * Builds a list of plans using a planner.
   * 
   * PLANNER: DPPlanner
   * -------------------
   * DPPlanner is a dynamic programming planner that uses a grid vision
   * of the world. Each state of the world is evaluated through its distance
   * from the target state.
   * Optionally DPPlanner can calculate an extra penalty using a cost function. 
   */
  def build(sense: WorldSense) : List[Plan] = {    
    // Considers the lack of resources.
    // Any resource not available has a cost punishment.
    
    val path = AStarPlanner((p1, p2) => {
      1.00 + space.cost(p2.x)(p2.y)
    }, (0,0), target).* 
    
    // Transforms each point into a movement action.
    val acts = path.points.map(ActPhy(_))
    return List(Plan(acts))
  }
}
