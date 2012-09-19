package usp.cognitio.msas.env
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.coal.VoteCoalition

import org.junit.Test
import org.junit.Assert._

class SessionSocTest extends Logging {

  @Test
  def test() {
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
    
    val session1 = SessionSoc(ag1, ag2, new VoteCoalition(ag2))
    val rc1_from2 = session1.allocate()
    assertEquals(Rc(0, 1, 0), rc1_from2)
    assertEquals(Rc(1, 0, 0), session1.alocs(ag2))
    assertTrue(session1.avaliate())

    val session2 = SessionSoc(agDummy, ag1, new VoteCoalition(ag1))
    session2.allocate()
    assertFalse(session2.avaliate())
  }

}