package usp.cognitio.msas.env
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.coal.KLinearSampleCoalitionGame

trait WorldSoc {
  val R: Int
  var coals: Map[MsasAg, Coalition] = Map.empty[MsasAg, Coalition]

  def ags : Array[MsasAg]

  /**
   * Avg agent´s wellfare.
   */
  def wellfare : Double = ags.map(_.u).sum/ags.size
  
  def createCoalition(ag: Ag) : Coalition = new KLinearSampleCoalitionGame(List(ag))
  def createCoalition(ags: List[Ag]) : Coalition = new KLinearSampleCoalitionGame(ags)
  
  def communicate(who: MsasAg, neigh: MsasAg): SessionSoc = SessionSoc(this, who, neigh, coals(neigh))
  
  /**
   * Coligates 'who' with 'neigh'.
   */
  def coligate(session: SessionSoc) : Boolean = {
    val ncoal = coals(session.neigh)
    if (ncoal.members.contains(session.who)) return false

    ncoal.add(session.who)
    ncoal.distribute()
    
    val wcoal = coals(session.who)
    wcoal.remove(session.who)
    
    coals += (session.who -> ncoal)
    return true
  }
  
}