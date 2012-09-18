package usp.cognitio.msas.simul.log
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.soc.Socialization

class RichLogAg(var ag:Ag) {
  def toId() : String = "[AG:" + ag.id + "]"
  def toU() : String = "[U:" + ag.asInstanceOf[Socialization].u + "]"

}