package usp.cognitio.msas.env
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.Rc
import usp.cognitio.math.alg.Point._

import org.junit.Test
import org.junit.Assert._

class EgoCogTest extends Logging {

  @Test
  def teste() {
    val world = new GridWorld(4)

    val ag1: MsasAg = new MsasAg(1, Rc(1, 0, 0)) {
      rcPi = Rc(0, 1, 0)
    }
    val ag2: MsasAg = new MsasAg(2, Rc(0, 1, 0)) {
      rcPi = Rc(1, 0, 0)
    }
    val agDummy: MsasAg = new MsasAg(99, Rc.nil) {
      rcPi = Rc.nil
    }
    
    ag1.init(world, world)
    ag2.init(world, world)
    agDummy.init(world, world)
    
    val plan = ag1.ecog.think(world.sense(ag1))
    debug(plan.path.toString)
    val xypath : List[(Int,Int)] = plan.path.points    
    assertEquals(List((0,0), (1,0), (1,1), (2,1), (2,2)), xypath)
    
    world.rcs(1)(1) = Rc(1,1,1)
    
    val plan2 = ag1.ecog.think(world.sense(ag1))
    debug(plan2.path.toString)
    val xypath2 : List[(Int,Int)] = plan2.path.points    
    assertEquals(List((0,0), (1,0), (2,0), (2,1), (2,2)), xypath2)
  }
  
}