package usp.cognitio.msas.coal
import usp.cognitio.msas.util.Logging
import org.junit.Test
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.soc.Socialization._
import org.junit.Assert.assertEquals
import usp.cognitio.msas.agent.cog.Cognition
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.Rc

class VoteCoalitionTest extends Logging {
  Socialization((ags: List[Ag]) => new VoteCoalition(ags))
  Rc.DIM = 3
  
  @Test
  def testAlone {
    val ag1: Ag = new Ag(1, Rc(1, 0, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 1, 0)
    }

    var coal: Coalition = new VoteCoalition(ag1)
    assertEquals(List(1.0), coal.shapley)
  }

  @Test
  def testZero {
    Rc.DIM = 2
    val ag1: Ag = new Ag(1, Rc(0, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 0)
    }

    var coal: Coalition = new VoteCoalition(ag1)
    assertEquals(List(1.0), coal.shapley)
    
    val ag2: Ag = new Ag(2, Rc(0, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 0)
    }    
    coal.add(ag2)
    assertEquals(List(0.5, 0.5), coal.shapley)

    ag2.rcPi = Rc(1,1)
    assertEquals(List(0.5, 0.5), coal.shapley)
    
    Rc.DIM = 3
  }  
  
  @Test
  def testHalfSh {
    val ag1: Ag = new Ag(1, Rc(1, 2, 1)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }
    val ag2: Ag = new Ag(2, Rc(2, 1, 1)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: Nil)
    assertEquals(List(0.5, 0.5), coal.shapley)
    debug("Shapley Value: " + coal.shapley)
  }

  @Test
  def testHalfShWithExcess {
    val ag1: Ag = new Ag(1, Rc(3, 3, 3)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }
    val ag2: Ag = new Ag(2, Rc(2, 1, 1)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: Nil)
    assertEquals(List(0.5, 0.5), coal.shapley)
    debug("Shapley Value: " + coal.shapley)
  }

  @Test
  def testOneThird {
    val ag1: Ag = new Ag(1, Rc(3, 3, 3)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }
    val ag2: Ag = new Ag(2, Rc(2, 1, 1)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }
    val ag3: Ag = new Ag(3, Rc(2, 1, 1)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 2, 2)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: ag3 :: Nil)
    assertEquals(List(0.33, 0.33, 0.33), coal.shapley)
    debug("Shapley Value: " + coal.shapley)
  }

  /**
   * <10; 6,4,2>
   *    Ag 1: sh = 0.5
   *    Ag 2: sh = 0.5
   *    Ag 3: sh = 0.0 !
   */
  @Test
  def testWooldrideDummy {
    Rc.DIM = 1
    val ag1: Ag = new Ag(1, Rc(6)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(4)
    }
    val ag2: Ag = new Ag(2, Rc(4)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(3)
    }
    val ag3: Ag = new Ag(3, Rc(2)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(3)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: ag3 :: Nil)
    assertEquals(List(0.5, 0.5, 0.0), coal.shapley)
    debug("Shapley Value: " + coal.shapley)
    
    Rc.DIM = 3
  }

  
    @Test
  def testWooldrideDummy2DIM {
    Rc.DIM = 2
    val ag1: Ag = new Ag(1, Rc(6,0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(4,0)
    }
    val ag2: Ag = new Ag(2, Rc(4,0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(3,0)
    }
    val ag3: Ag = new Ag(3, Rc(2,0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(3,0)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: ag3 :: Nil)
    assertEquals(List(0.41, 0.41, 0.16), coal.shapley)
    debug("Shapley Value: " + coal.shapley)
    
    Rc.DIM = 3
  }
  
  /**
   * O mesmo exemplo anterior, agora com mais um jogador.
   * <10; 6,4,2,8>
   */
  def testWooldridgeNonDummy {
    val ag1: Ag = new Ag(1, Rc(3, 3, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 1, 1)
    }
    val ag2: Ag = new Ag(2, Rc(2, 1, 1)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(1, 1, 0)
    }
    val ag3: Ag = new Ag(3, Rc(2, 0, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(1, 1, 0)
    }
    val ag4: Ag = new Ag(3, Rc(2, 2, 4)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(1, 1, 0)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: ag3 :: ag4 :: Nil)
    assertEquals(coal.shapley, List(0.25, 0.25, 0.08, 0, 41))
    debug("Shapley Value: " + coal.shapley)
  }

  @Test
  def testAddRemove {
    val ag1: Ag = new Ag(1, Rc(1, 2, 1)) with Socialization {
      var rcPi = Rc(2, 2, 2)
    }
    val ag2: Ag = new Ag(2, Rc(2, 1, 1)) with Socialization {
      var rcPi = Rc(2, 2, 2)
    }

    var coal: Coalition = ag1.coalition
    coal.add(ag2)
    assertEquals(coal.shapley, List(0.5, 0.5))
    debug("Shapley Value: " + coal.shapley)
  }  
 
  @Test
  def testAlocate {
    var oldDim = Rc.DIM
    Rc.DIM = 2
    
    val ag1: Ag = new Ag(1, Rc(0, 2)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 0)
    }
    val ag2: Ag = new Ag(2, Rc(2, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 2)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: Nil)
    assertEquals(coal.shapley, List(0.5, 0.5))
    debug("Shapley Value: " + coal.shapley)
    
    var alocs = coal.alocate()
    assertEquals(Rc(2,0), alocs(ag1))
    assertEquals(Rc(0,2), alocs(ag2))
    
    Rc.DIM = oldDim    
  }

  @Test
  def testRestore {
    var oldDim = Rc.DIM
    Rc.DIM = 2
    val ag1: Ag = new Ag(1, Rc(0, 3)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(2, 0)
    }
    val ag2: Ag = new Ag(2, Rc(2, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 2)
    }

    var coal: Coalition = new VoteCoalition(ag1 :: ag2 :: Nil)
    assertEquals(List(0.5, 0.5), coal.shapley)    
    var alocs = coal.alocate()
    coal.distribute()
    
    assertEquals(Rc(2,1), alocs(ag1))
    assertEquals(Rc(0,2), alocs(ag2))
    
    Rc.DIM = oldDim
  }  
}