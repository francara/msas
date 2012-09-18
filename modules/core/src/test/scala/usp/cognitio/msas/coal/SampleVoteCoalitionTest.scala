package usp.cognitio.msas.coal
import scala.math.min
import scala.util.Random._
import org.junit.Assert.assertEquals
import org.junit.Test
import usp.cognitio.msas.agent.cog.Cognition
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.simul.log.RichLogString
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.simul.log.RichLogAg
import usp.cognitio.msas.Rc

class SampleVoteCoalitionTest extends Logging {
  private val MAX_RC = 5

  //  Socialization((ags: List[Ag]) => new SampleVoteCoalition(ags,2.5,0.5))
  Socialization((ags: List[Ag]) => new KSampleVoteCoalition(ags))
  Rc.DIM = 3
  
//  @Test
  def testN2 {
    val ag1: Ag = new Ag(1, Rc(100, 300, 45)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(200, 200, 100)
    }
    val ag2: Ag = new Ag(2, Rc(300, 100, 55)) with Cognition with Socialization {
      val world = new GridWorld(1)
      rcPi = Rc(200, 200, 100)
    }

    var coal: Coalition = new KLinearSampleCoalitionGame(ag1 :: ag2 :: Nil)
    //    var coal: Coalition = new SampleVoteCoalition(ag1 :: ag2 :: Nil, 1.5, 0.25)
    val shp = coal.shapley
    debug("Shapley Value: " + shp)
    shp.foreach(v => assertEquals(0.5, v, 0.02))
  }

  @Test
  def testN20 {
    val ags = Socialization(Ag(40,10,9), 0.9*10, 9);

    var coal: Coalition = new KLinearSampleCoalitionGame(ags)
    val shp = coal.shapley
    debug("Shapley Value: " + shp)
    for (i <- 0 to ags.size-1){
      val ag = ags(i)
      debug(ag.toId * "Rc" * ag.rc.toString *| ("Total", ag.rc.sum) *| ("Shp",shp(i)))
    }
    debug("Coalition" * "Rc" * coal.rc.toString * "Pi" * coal.rcPi.toString)    
  }
  
//  @Test
  def testMore {
    val ags = Socialization(Ag(
      List(
        List(6, 50, 10),
        List(4, 30, 11),
        List(2, 10, 12))), 4, 1);

//    var t_coal = new KTestSampleCoalition(ags)
    var t_coal = new KTestLinearSampleCoalition(ags)
    debug("Coalition Rc:" + t_coal.rc)
    debug("Coalition RcPi:" + t_coal.rcPi)
    def calcPL(ag: Ag, k: Int) : (Double,Double,Double) = {
      var pl_1_1 = t_coal.calcPL(ag, 1, k)
      var pl_2_1 = t_coal.calcPL(ag, 2, k)
      var pl_3_1 = t_coal.calcPL(ag, 3, k)

      debug(ag.toId
        *| ("PL_1_1", pl_1_1)
        *| ("PL_2_1", pl_2_1)
        *| ("PL_3_1", pl_3_1))
        
      (pl_1_1, pl_2_1, pl_3_1)
    }

    val (ag1_1_, ag1_2_1, ag1_3_1) = calcPL(ags(0), 0)
    val (ag2_1_, ag2_2_1, ag2_3_1) = calcPL(ags(1), 0)
    val (ag3_1_, ag3_2_1, ag3_3_1) = calcPL(ags(2), 0)

    
    
    //    var coal: Coalition = new KSampleVoteCoalition(ags)
    //    val shp = coal.shapley
    //    debug("Coalition Rc:" + coal.rc)
    //    debug("Coalition RcPi:" + coal.rcPi)
    //    debug("Shapley Value: " + shp)
  }

  implicit def StringToLog(msg: String): RichLogString = new RichLogString(msg)
  implicit def AgToLog(ag: Ag): RichLogAg = new RichLogAg(ag)

}

class KTestSampleCoalition(_mbs: List[Ag]) extends KSampleVoteCoalition(_mbs) {

  def calcPL(ag: Ag, X: Int, k: Int): Double = {
    trac(ag.toId * this.toKEsh(X, k))

    val pl = PL(ag, X, k)

    return pl
  }

  implicit def StringToLog(msg: String): RichLogString = new RichLogString(msg)
  implicit def AgToLog(ag: Ag): RichLogAg = new RichLogAg(ag)

}

class KTestLinearSampleCoalition(_mbs: List[Ag]) extends KLinearSampleCoalitionGame(_mbs) {

  def calcPL(ag: Ag, X: Int, k: Int): Double = {
    trac(ag.toId * this.toKEsh(X, k))

    val pl = PL(ag, X, k)

    return pl
  }

  implicit def AgToLog(ag: Ag): RichLogAg = new RichLogAg(ag)

}

