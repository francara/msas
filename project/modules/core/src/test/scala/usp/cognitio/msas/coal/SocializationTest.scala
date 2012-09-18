package usp.cognitio.msas.coal
import org.junit.Assert._
import org.junit.Test
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.cog.Cognition
import usp.cognitio.msas.agent.soc.Socialization._
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.Rc

class SocializationTest extends Logging {
  Socialization((ags: List[Ag]) => new VoteCoalition(ags))
  Rc.DIM = 3

  @Test
  def testConsume {
    val ag1: Ag = new Ag(1, Rc(1, 2, 1)) with Socialization {
      var rcPi = Rc(2, 2, 2)
    }
    val ag2: Ag = new Ag(2, Rc(2, 1, 1)) with Socialization {
      var rcPi = Rc(1, 0, 2)
    }

    assertEquals(Rc.nil, ag1.consume(Rc.nil, 3))
    assertEquals(Rc(2, 1, 0), ag1.consume(Rc(2, 3, 4), 3))
    assertEquals(Rc(1, 0, 2), ag2.consume(Rc(2, 3, 4), 3))
  }

  @Test
  def testAvaliation {
    val ag1: Ag = new Ag(1, Rc(1, 0, 0)) with Socialization {
      var rcPi = Rc(0, 1, 0)
    }
    val ag2: Ag = new Ag(2, Rc(0, 1, 0)) with Socialization {
      var rcPi = Rc(1, 0, 0)
    }
    val agDummy: Ag = new Ag(99, Rc.nil) with Socialization {
      var rcPi = Rc.nil
    }

    assertEquals(ag1.u, ag1.avaliation(agDummy), 0)
    assertEquals(List(ag1), ag1.coalition.members)

    assertEquals(1.0, ag1.avaliation(ag2), 0)
    assertEquals(1.0, ag2.avaliation(ag1), 0)

    // Ag2 needs 2 resources from the coalition.
    // The utility of a colition with ag1 should be 0.5.
    ag2.rcPi = Rc(2, 0, 0)
    assertEquals(1.0, ag1.avaliation(ag2), 0)
    assertEquals(0.5, ag2.avaliation(ag1), 0)
  }

  @Test
  def testColigate {
    val ag1: Ag = new Ag(1, Rc(1, 0, 0)) with Socialization {
      var rcPi = Rc(0, 1, 0)
    }
    val ag2: Ag = new Ag(2, Rc(0, 1, 0)) with Socialization {
      var rcPi = Rc(1, 0, 0)
    }

    ag1.coligate(ag2)
    assertEquals(Rc(0, 1, 0), ag1.rc)
    assertEquals(Rc(1, 0, 0), ag2.rc)
  }

  @Test
  def testAbandon {
    val ag1: Ag = new Ag(1, Rc(1, 0, 0)) with Socialization {
      var rcPi = Rc(0, 1, 0)
    }
    val ag2: Ag = new Ag(2, Rc(0, 1, 0)) with Socialization {
      var rcPi = Rc(1, 0, 0)
    }

    ag1.coligate(ag2)
    assertEquals(Rc(0, 1, 0), ag1.rc)
    assertEquals(Rc(1, 0, 0), ag2.rc)
    
    ag2.abandon()
    assertEquals(Rc(0, 1, 0), ag1.rc)
    assertEquals(Rc(1, 0, 0), ag2.rc)
    
  }

}