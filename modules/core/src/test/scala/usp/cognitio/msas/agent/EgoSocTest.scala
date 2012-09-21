package usp.cognitio.msas.agent
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.Rc
import usp.cognitio.msas.coal.VoteCoalition

import org.junit.Test
import org.junit.Assert._

class EgoSocTest extends Logging {

  @Test
  def teste() {
    Rc.DIM=3
    val world = new GridWorld(4)

    val ag1: MsasAg = new MsasAg(1, Rc(1, 0, 0)) {
      rcPi = Rc(0,0,0)
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
    
    for (i <- 0 to world.R-1) world.rcs(1)(i) = Rc(0,1,0)
    world.enter(ag1, 0, 0)
    world.enter(ag2, 3, 3)
    
    ag1.act(world.sense(ag1))
    assertEquals(Rc(0, 1, 0), ag1.rc)
    assertEquals(Rc(1, 0, 0), ag2.rc)
    ag2.act(world.sense(ag2))
  }
}