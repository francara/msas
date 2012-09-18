package usp.cognitio.msas.coal
import org.junit.Assert.assertEquals
import org.junit.Test
import usp.cognitio.msas.agent.soc.Socialization._
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.cog.Cognition
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.Rc

class CoalitionTest extends Logging {
  Socialization((ags: List[Ag]) => new VoteCoalition(ags))

  @Test
  def testDummyShapley() {
    val ag1: Ag = new Ag(1)
    val ag2: Ag = new Ag(2)

    /*
     * Coalition with a fixed shapley value.
     */
    var coal: Coalition = new Coalition(ag1) {
      // Sh := index of agent.
      override def shapley: List[Double] = mbs.map(el => mbs.indexOf(el).asInstanceOf[Double])
      override def v = 0.0
    }
    assertEquals(coal.shapley(0), 0.0, 0)

    coal.add(ag2)
    assertEquals(coal.shapley(0), 0.0, 0)
    assertEquals(coal.shapley(1), 1.0, 0)
  }

  @Test
  def testTarget() {
    /*
     * Agents with fixed plan.
     */
    val ag1: Ag = new Ag(1, Rc(1, 3, 5)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(1, 1, 1)
    }
    val ag2: Ag = new Ag(2, Rc(2, 4, 6)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(1, 1, 1)
    }

    var coal: Coalition = new Coalition(ag1 :: ag2 :: Nil) {
      override def shapley: List[Double] = mbs.map(el => 0.00)
      override def v = 0.0
    }
    assertEquals(coal.rc.sum, ag1.rc.sum + ag2.rc.sum)
    assertEquals(coal.rcPi, ag1.rcPi + ag2.rcPi)
  }

  @Test
  def testRcDiv {
    val ag1: Ag = new Ag(1, Rc(0, 4, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc()
    }
    val ag2: Ag = new Ag(2, Rc(2, 0, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc()
    }

    /* Shapley 0.5 for each. */
    var coal1: Coalition = new Coalition(ag1 :: ag2 :: Nil) {
      override def shapley: List[Double] = mbs.map(el => 0.5)
      override def v = 0.0
    }
   
    assertEquals(List((ag1,3),(ag2,3)), coal1.rcDiv)

    /* Shapley (0.7, 0.3). */
    var coal2: Coalition = new Coalition(ag1 :: ag2 :: Nil) {
      override def shapley: List[Double] = mbs.map(el => if (el == ag1) 0.7 else 0.3)
      override def v = 0.0
    }

    assertEquals(List((ag1,4),(ag2,2)), coal2.rcDiv)
    assertEquals(4, coal2.rcDiv(ag1))
    assertEquals(2, coal2.rcDiv(ag2))
    
  }
}