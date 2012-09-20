package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.agent.cog.Plan._
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.agent.cog.dp.DPPlanner
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.plan.AStarPlanner
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.cog.plan.Space
import usp.cognitio.msas.Rc

trait PlanProspector {
  def space : Space
  def target : Point
  def rc: Rc

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
    
    val consume = Rc(rc.resources)
    val path = AStarPlanner((p1, p2) => {
      1.00 + space.cost(p2.x)(p2.y) + penalty(sense, consume, p2)
    }, (0,0), target).* 
    
    // Transforms each point into a movement action.
    val acts = path.points.map(ActPhy(_))
    return List(Plan(acts))
  }
  
  /**
   * Calculates a penalty for lack of resources.
   * 
   * @param sense WorldSense
   * @param avail Available resources
   * @param p coordinates
   */
  def penalty(sense: WorldSense, avail: Rc, p: Point) : Double = {
    // Required resource
    val req = sense.rcs(p.x)(p.y)
    
    if ((avail ^- req).sum == 0) return 0.00
    else return 4.00
  }
}
