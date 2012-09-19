package usp.cognitio.msas.env
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.coal.KLinearSampleCoalitionGame

trait WorldSoc {
  val N: Int
  val neighs: Array[MsasAg] = Array.empty[MsasAg]
  val coal: Map[MsasAg, Coalition] = Map.empty[MsasAg, Coalition]

  def createCoalition(ag: Ag) : Coalition = new KLinearSampleCoalitionGame(List(ag))
  def createCoalition(ags: List[Ag]) : Coalition = new KLinearSampleCoalitionGame(ags)
  
  def communicate(who: MsasAg, neigh: MsasAg): SessionSoc = SessionSoc(who, neigh, coal(neigh))
  
  def sense(ag: Ag): WorldSense
}