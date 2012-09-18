package usp.cognitio.msas
import org.junit.Test

import usp.cognitio.msas.agent.res.RcDistributor
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.util.Logging

class RcDistributorTest extends Logging {

  @Test
  def testDistrib() {
    val ags : List[Ag] = List( Ag(1, Rc()), Ag(2, Rc()) )
    val distributor = new RcDistributor(ags, 10, 5, 2)
    
    debug("Ag1: " + distributor.agDistrib(ags(0)) + " - Rc: " + distributor.agRcs(ags(0)) ) 
    debug("Ag2: " + distributor.agDistrib(ags(1)) + " - Rc: " + distributor.agRcs(ags(1)) ) 
  }
  
}