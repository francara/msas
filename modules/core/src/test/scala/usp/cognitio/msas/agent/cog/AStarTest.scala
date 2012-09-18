package usp.cognitio.msas.agent.cog
import org.junit.Test
import org.junit.Assert._
import usp.cognitio.math.alg.Point._
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.cog.plan.AStarPlanner
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.cog.plan.Space

class AStarTest extends Logging {
  
  @Test
  def test() {
    Point.MAX = 4
    val space = Space()
    
    debug("Space => " + space)
    
    val cost: Array[Array[Double]] = Array.tabulate(4,4)((x, y) => 0.00)
    
    val path = AStarPlanner((p1, p2) => {
      1.00 + space.cost(p2.x)(p2.y)
    }, (0,0), (2,2)).*
    
    debug(path.toString)
    val xypath : List[(Int,Int)] = path.points
    assertEquals(List((0,0), (1,0), (1,1), (2,1), (2,2)), xypath)
    
    // Add cost.
    space.punish((1,1))
    debug("Space => " + space)
    
    val path2 = AStarPlanner((p1, p2) => {
      1.00 + space.cost(p2.x)(p2.y)
    }, (0,0), (2,2)).*
    debug(path2.toString)
    val xypath2 : List[(Int,Int)] = path2.points    
    assertEquals(List((0,0), (1,0), (2,0), (2,1), (2,2)), xypath2)
    
    space.punish((2,0))
    space.punish((1,2))
    debug("Space => " + space)
    
    val path3 = AStarPlanner((p1, p2) => {
      1.00 + space.cost(p2.x)(p2.y)
    }, (0,0), (2,2)).*
    debug(path3.toString)
    val xypath3 : List[(Int,Int)] = path3.points    
    assertEquals(List((0,0), (0,1), (0,2), (0,3), (1,3), (2,3), (2,2)), xypath3)   
  }
  
}