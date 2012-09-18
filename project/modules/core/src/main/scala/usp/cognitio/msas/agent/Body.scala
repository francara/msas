package usp.cognitio.msas.agent
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.env.WorldSoc

case class Body (
  val ag : MsasAg,
  val phy : WorldPhy,
  val soc : WorldSoc) {
  
  def act(action: ActPhy):Boolean = {
    true
  }
  
  def act(action: ActSoc):Boolean = {
    true
  }
}