package usp.cognitio.msas.agent
import usp.cognitio.msas.agent.EgoCog
import usp.cognitio.msas.env.WorldPhy
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.env.WorldSoc
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.agent.cog.NullPlan
import usp.cognitio.msas.agent.cog.SingletonPlan
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.env.specific.PlanBehaviour

class MsasAg(_id: Long, _rc: Rc) extends Ag(_id, _rc) 
	with Player with PlanBehaviour {
  var ecog: EgoCog = null
  var esoc: EgoSoc = null
  var body: Body = null

  var plan: Plan = NullPlan()

  var rcPi: Rc = Rc()

  var target: Point = null
  
  var qtdAval = 0
  var qtdColigate = 0
  var qtdReplan = 0

  def u : Double = esoc.u
  def u(al: Rc): Double = esoc.u(al)

  def init(wphy: WorldPhy, wsoc: WorldSoc) {
    body = Body(this, wphy, wsoc)
    ecog = EgoCog(this)
    esoc = EgoSoc(this)
    
    target = Point(body.phy.R/2, body.phy.R/2)
  }

  def coalition: Coalition = esoc.coalition
  def consume(rcCoal: Rc, q: Int): Rc = esoc.consume(rcCoal, q)
 
  def ag : MsasAg = this

  override def consume(cRc: Rc) : Rc = {
    rcPi = rcPi - cRc
    return super.consume(cRc)
  }
  
}