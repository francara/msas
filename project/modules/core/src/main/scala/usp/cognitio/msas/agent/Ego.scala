package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.Rc

abstract case class Ego(val ag:MsasAg) {
  def rc : Rc
  def rcPi : Rc
}
