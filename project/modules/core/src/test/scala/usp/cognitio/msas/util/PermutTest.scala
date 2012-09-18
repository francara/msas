package usp.cognitio.msas.util
import org.junit.Test
import usp.cognitio.msas.agent.Ag

class PermutTest extends Logging {

  @Test
  def testPermutSize2 {
    val ag1 = new Ag(1)
    val ag2 = new Ag(2)
    val permut = new Permute(ag1 :: ag2 :: Nil)
    
    debug("Size 2: " + permut.go.toString)
  }
  
  @Test
  def testPermutSize3 {
    val ag1 = new Ag(1)
    val ag2 = new Ag(2)
    val ag3 = new Ag(3)
    val permut = new Permute(ag1 :: ag2 :: ag3 :: Nil)
    
    debug("Size 3: " + permut.go.toString)
  }
 
  @Test
  def testPermutSize4 {
    val ag1 = new Ag(1)
    val ag2 = new Ag(2)
    val ag3 = new Ag(3)
    val ag4 = new Ag(4)
    val permut = new Permute(ag1 :: ag2 :: ag3 :: ag4 :: Nil)
    
    debug("Size 4: " + permut.go.toString)
  }  
}