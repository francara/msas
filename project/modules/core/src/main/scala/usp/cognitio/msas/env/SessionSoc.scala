package usp.cognitio.msas.env
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.coal.Coalition

abstract class SessionSoc(who: MsasAg, neigh: Ag) {
  def coalition : Coalition
  
  def allocation : Rc = {
    null
  }
  
  def aval : Boolean
    
}