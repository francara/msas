package usp.cognitio.msas.agent.cog.dp
import org.junit.Assert._
import org.junit.Test
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.cog.plan.Space

class DPPlannerTest extends Logging {

  @Test
  def testSimple {
    debug("*********   Test Simple   *********")
    val space = new Space(4)
    val planner = new DPPlanner(space) 

    debug("State: " + space.toString)
    debug("Cost: " + planner.toString)
    
    assertEquals(List((0,0),(1,0),(1,1),(2,1), (2,2)), planner.path(0,0))
    assertEquals(List((1,0),(1,1),(2,1), (2,2)), planner.path(1,0))
    assertEquals(List((1,1),(2,1), (2,2)), planner.path(1,1))
    assertEquals(List((3,3),(2,3), (2,2)), planner.path(3,3))
  }

  @Test
  def TestBranch {
    debug("*********   Test Branch   *********")
    val space = new Space(4)
    val planner = new DPPlanner(space) 

    /*
     * Tests if the branch happens when a replanning is issued.
     * 
     * The branch affects the last cell in the path. This cell receives
     * a penalty to obbly the next planning avoid her.
     */
    val path = planner.path(0,0)
    planner.unpath(path)
    
    assertTrue(planner.hasPenalty(2,1))
    debug("Cost after penalty: " + planner.toString)    
    assertEquals(List((0,0),(1,0),(1,1),(1,2), (2,2)), planner.path(0,0))

    planner.unpath(planner.path(0,0))
    assertTrue(planner.hasPenalty(1,2))
//    assertEquals(List((0,0),(1,0),(1,1),(1,2), (2,2)), planner.path(0,0))
//    debug(planner.path(0,0).toString)
  }


}