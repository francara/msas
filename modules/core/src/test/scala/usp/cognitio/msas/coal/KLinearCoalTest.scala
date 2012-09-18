package usp.cognitio.msas.coal
import usp.cognitio.msas.agent.cog.Cognition
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.GridWorld
import org.junit.Test
import org.junit.Assert._
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.Rc

class KLinearCoalTest extends Logging {
  Socialization((ags: List[Ag]) => new KSampleVoteCoalition(ags))  
  Rc.DIM = 5

  @Test
  def testNullShp {
    val ag9: Ag = new Ag(9, Rc(0, 0, 9, 11, 12)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(19, 13, 0, 0, 0)
    }
    val ag22: Ag = new Ag(22, Rc(0, 17, 8, 12, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(13, 0, 0, 16, 0)
    }
    val ag18: Ag = new Ag(18, Rc(12, 4, 13, 8, 0)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(0, 0, 0, 0, 13)
    }

    
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9] Rc{[13,21,30,31,12]} / Pi{[32,13,0,16,12]}
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9]       [AG:22] Rc{[0,17,8,12,0]} / Pi{[13,0,0,16,0]}
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9]       [AG:18] Rc{[13,4,13,8,0]} / Pi{[0,0,0,0,12]}
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9]       [AG:9] Rc{[0,0,9,11,12]} / Pi{[19,13,0,0,0]}
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9] Shapley (9,1.0),(18,0.0),(22,0.0)
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9] Division (22,0),(18,0),(9,107)
//2012-07-29 16:02:57 [IT:13] [COAL:22,18,9] Alocation (9,[13,13,0,0,0]),(18,[0,0,0,0,0]),(22,[0,0,0,0,0])
    var k_coal: Coalition = new KLinearSampleCoalitionGame(ag9 :: ag18 :: ag22 :: Nil)
    //    var coal: Coalition = new SampleVoteCoalition(ag1 :: ag2 :: Nil, 1.5, 0.25)
    val shp = k_coal.shapley
    debug("Shapley Value: " + shp)
    
    var p_coal : Coalition = new VoteCoalition(ag22 :: ag18 :: ag9 :: Nil)
    val p_shp = p_coal.shapley
    debug("P_COAL Shapley Value: " + p_shp)
  }

}