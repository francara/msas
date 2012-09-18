package usp.cognitio.msas.env
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.env.WorldSense

trait WorldSoc {
  def communicate(who: MsasAg, neigh: Ag) : SessionSoc
  def coligate(who: Ag, coal: Coalition) : Boolean
  def abandon(who: Ag, coal: Coalition) : Boolean
  
  def sense(ag:Ag) : WorldSense
}