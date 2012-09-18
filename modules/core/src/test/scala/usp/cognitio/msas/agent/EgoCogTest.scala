package usp.cognitio.msas.agent
import org.junit.Test
import usp.cognitio.msas.Rc

class EgoCogTest {

  trait MockPlanner extends PlanProspector
  
  @Test
  def test() {
    val ag : MsasAg = new MsasAg(1, Rc())
    val ego = new EgoCog(ag) with MockPlanner
  }
  
}