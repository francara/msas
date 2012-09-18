package usp.cognitio.msas.agent.cog.plan
import org.junit.Test

import usp.cognitio.msas.util.Logging

class DPSpaceTest extends Logging {

  @Test
  def testSimple {
    debug("*********   Test Simple   *********")
    val space = new Space(4)

    debug("State: " + space.toString)

    // Every neighboor around the target should have a higher state value.
    // The values also should be the same.
    
    
  }

}