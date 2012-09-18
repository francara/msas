package usp.cognitio.msas.agent.cog
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.agent.cog.dp.DPPlanner
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.cog.plan.Space
import usp.cognitio.msas.Rc

trait Cognition extends Ag {

  /** The agent needs a vision of the world. */
  val world: GridWorld
  var rcPi: Rc = Rc.nil

  lazy val space: Space = new Space(world.N)
  lazy val planner: DPPlanner = new DPPlanner(space)

  var path: List[(Int, Int)] = List()

  def buildPlan: Unit = {

    /*
     * Update the cost to consider available resources.
     */
    val rcPath = world.toRc(path)
    path.indices.foreach(i => if (rcPath(i) > rc(i)) planner.punish(path(i)) )
    
    /*
     * Selects the best path to the target.
     */
    path = planner.path(position.x, position.y)

    rcPi = world.toRc(path)
  }

  def required: Rc = rcPi
}
