package usp.cognitio.msas.env.spy
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.math.alg.Point
import org.apache.log4j.Logger
import usp.cognitio.msas.env.spy.PlanSpy
import usp.cognitio.msas.env.specific.PlanCompleteActAllBehaviour
import usp.cognitio.msas.env.specific.PlanBehaviour

trait PlanSpy extends GridWorld {

  val logger = Logger.getLogger(getClass().getName());
  
  val spy: SpyFile

  /*
   * SPIES.
   */
  override def move(ag: Ag, pos: Point): Boolean = {
    val moved = super.move(ag, pos)
    if (moved) spy.moving(ag, pos)
    
    return moved
  }

  override def communicate(who: MsasAg, neigh: MsasAg): SessionSoc = new SessionSoc(this, who, neigh, coals(neigh)) {
    override def doubleWin(): Boolean = {
      logger.debug("Avaliating: " + who + " => " + neigh)
      super.doubleWin()
    }
  }

  override def coligate(session: SessionSoc): Boolean = {
    val coligated = super.coligate(session)
    if (coligated) {
      spy.coligating(session.who, session.neigh, session.alocs)
      logger.debug("Coligated: " + session.who + " => " + session.neigh)
    }
    coligated
  }

  def finish() { spy.close() }

}