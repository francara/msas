package usp.cognitio.msas.env
import scala.Array.canBuildFrom

import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.coal.KLinearSampleCoalitionGame
import usp.cognitio.msas.coal.KVoteCoalition
import usp.cognitio.msas.env.SessionSoc

trait WorldSoc {
  val R: Int
  var coals: Map[MsasAg, Coalition] = Map.empty[MsasAg, Coalition]

  def ags : Array[MsasAg]

  /**
   * Avg agent´s wellfare.
   */
  def lack : Int = ags.map(_.rcMinus.sum).sum
  def wellfare : Double = ags.map(_.u).sum/ags.size
  
  def createCoalition(ag: Ag) : Coalition = new KLinearSampleCoalitionGame(List(ag))
  def createCoalition(ags: List[Ag]) : Coalition = 
    if (ags.size > 7) new KLinearSampleCoalitionGame(ags)
    else new KVoteCoalition(ags)
  
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