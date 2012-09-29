package usp.cognitio.msas.env
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.soc.Aval

import scala.collection._

case class SessionSoc(val wsoc: WorldSoc, val who: MsasAg, val neigh: MsasAg, val coalition: Coalition) {
  var allocated = false
  var alocs: Map[Ag, Rc] = Map.empty[Ag, Rc]
  
  /** Wellfare before the agent joins neigh. */
  var beforeWellfare = 0.00
  /** Wellfare aftger the agent joins neigh. */
  var afterWellfare = 0.00
  /** Agents utility before joins neigh. */
  var beforeU = 0.00
  /** Agents utility after joins neigh. */
  var afterU = 0.00
  
  def allocate(): Rc = {
    if (!allocated) doAllocation()
    return alocs(who)
  }

  def avaliate(): Boolean = {
    if (!allocated) doAllocation()

    var doubleWin = true
    /*
     * Double-win test.
     */
    // Agent increment
    if (doubleWin && afterU > beforeU) doubleWin = true
    else doubleWin = false
      
    // Wellfare increment
    if (doubleWin && afterWellfare > beforeWellfare) doubleWin = true
    else doubleWin = false
    
    // Coalition - {i} wellfare increment
    if (doubleWin && coalition.u(alocs) - coalition.u > -1*(who.u(alocs(who)) - who.u)) true
    else doubleWin = false
    
    return doubleWin
  }

  def doAllocation() {
    val isMember: Boolean = if (coalition.members.contains(who)) true else false

    beforeWellfare = coalition.u
    beforeU = who.u

    if (!isMember) coalition.add(who)
    this.alocs = coalition.alocate()
    
    afterWellfare = coalition.u(alocs)
    afterU = who.u(alocs(who))
    
    if (!isMember) coalition.remove(who)
    if (!alocs.contains(neigh) || !alocs.contains(who)) {
      error("Ag not found in coalition: this: " + neigh + ", who: " + who)
    }

    this.allocated = true
  }

  def coligate(): Boolean = {
    return wsoc.coligate(this)
  }

}