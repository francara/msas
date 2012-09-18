package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.EgoCog
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.env.WorldSoc
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.cog.Plan

class MsasAg(_id:Long, _rc:Rc) extends Ag(_id,_rc) {
  val ecog : EgoCog = EgoCog(this)
  val esoc : EgoSoc = EgoSoc(this)
  var body : Body = null
  
  var plan : Plan = null
  
  var rcPi : Rc = Rc()
  
  def init(wphy: WorldPhy, wsoc: WorldSoc) {
    body = Body(this, wphy, wsoc)
  }
  
  def act(sense: WorldSense) {
    val nplan = ecog.act(sense)
    merge(nplan, plan)
    
    rcPi = ecog.mapit(sense, plan)
    
    esoc.act(sense, plan)
    
    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }
  
  def merge(plan1: Plan, plan2: Plan) : Plan = {
    return plan1
  }
  
}