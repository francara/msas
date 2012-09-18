package usp.cognitio.msas.agent.soc.cen
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import usp.cognitio.msas.agent.cog.Cognition
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.coal.VoteCoalition
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc

/**
 * Tests coalitions with |K| = 2.
 * Resources with dimension 2.
 */
class CoalK2Test extends Logging {
  /** Socialized by a vote game. */
  Socialization((ags: List[Ag]) => new VoteCoalition(ags))

  /** Dimensions. */
  Rc.DIM = 2

  /**
   * Full complementarity: C[a,b] = 1
   */
  @Test
  def testFullComplementarity {
    val a: Ag = new Ag(1, Rc(2, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 2)
    }
    val b: Ag = new Ag(2, Rc(0, 2)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 0)
    }

    var ub : Double = a.avaliation(b)
    if (ub >= b.u) a.coligate(b)
    
    assertTrue(a.coalition.members.contains(b))
    assertTrue(b.coalition.members.contains(a))
    
    /*
     * A third member would not be added, becaouse there is not
     * any resource missing.
     */
    val c: Ag = new Ag(3, Rc(1, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 1)
    }

    var uc : Double = a.avaliate(c).uAg
    var c_u : Double = c.u
    assertEquals(0.00, uc, 0)
    if (uc > c.u) a.coligate(c)
    
    assertTrue(a.coalition.members.contains(b))
    assertTrue(b.coalition.members.contains(a))
    assertFalse(a.coalition.members.contains(c))
    assertFalse(c.coalition.members.contains(a))
    
    val d: Ag = new Ag(4, Rc(0, 2)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 0)
    }    

    var ud : Double = a.avaliate(d).uAg
    var d_u = d.u
    if (ud > d.u) a.coligate(d)
    
    // Utility of 'b' (it should not be changed by additon of 'd').
    
    assertTrue(a.coalition.members.contains(b))
    assertTrue(b.coalition.members.contains(a))
    assertFalse(a.coalition.members.contains(d))
    assertFalse(d.coalition.members.contains(a))
  }
  
  /**
   * Partial complementarity: C[a,b] = 0.5
   */
  @Test
  def testHalfComplementarity {
    val a: Ag = new Ag(1, Rc(2, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 4)
    }
    val b: Ag = new Ag(2, Rc(0, 2)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(4, 0)
    }    
    
    var ub : Double = a.avaliation(b)
    if (ub >= b.u) a.coligate(b)
    
    assertTrue(a.coalition.members.contains(b))
    assertTrue(b.coalition.members.contains(a))
    assertEquals(Rc(0,2), a.rc)
    assertEquals(Rc(2,0), b.rc)
    
    a.rc = a.rc + Rc(3,0)
    assertEquals(Rc(3,2), a.rc)
    
    a.coalition.distribute()
    assertEquals(Rc(3,0), b.rc)
    assertEquals(Rc(2,2), a.rc)
    
  }
  
  /**
   * Partial complementarity: ap[a,b] = 0.6, ap[b,a]=0.4
   */
  @Test
  def testSixtyFourthyComplementarity {
  }
  
}
