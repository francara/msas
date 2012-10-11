package usp.cognitio.msas.env.spy.io
import usp.cognitio.msas.util.Logging
import org.junit.Test
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.Rc
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.env.spy.SpyFile

class PhySpyFileTest extends Logging {

  @Test
  def testWriteMove() {
    val ag = new MsasAg(1, Rc())
    
    val phy : SpyFile = SpyFile()
    phy.moving(ag, Point(1,2))
    phy.close()
  }
  
}