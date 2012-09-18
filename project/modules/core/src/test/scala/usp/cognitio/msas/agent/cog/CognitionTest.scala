package usp.cognitio.msas.agent.cog
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.util.Logging
import org.junit.Test
import usp.cognitio.msas.Rc

class CognitionTest extends Logging {
  val world : GridWorld = new GridWorld(4)
  
  @Test
  def init {
    /*
     * Initialize the world.
     */
    world.putRc(1,1,new Rc(3 :: 0 :: 0 :: Nil))
    
    /*
     * Verify paths and resources.
     */
  }
  
}