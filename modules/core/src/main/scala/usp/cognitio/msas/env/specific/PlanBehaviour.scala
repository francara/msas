package usp.cognitio.msas.env.specific
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.EgoCog
import usp.cognitio.msas.agent.EgoSoc
import usp.cognitio.msas.agent.cog.NullPlan
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.Body
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.SingletonPlan
import usp.cognitio.msas.agent.ActPhy
import usp.cognitio.msas.agent.ActSoc
import usp.cognitio.msas.agent.Ag
import org.apache.log4j.Logger
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.agent.Traceable
import usp.cognitio.msas.agent.MindTraceable

trait PlanBehaviour extends MindTraceable {  
  var STOP_WHEN_STUCKED = 0
  var STOP_WHEN_INSUFFICENT = 1

  var ecog: EgoCog
  var esoc: EgoSoc
  var body: Body

  var plan: Plan

  var rc: Rc
  var rcPi: Rc

  var target: Point
  
  /**
   * Number of iterations without wellfare evolution.
   */
  val lambda = 4
  var wellfareStucked = 0
  var wellfare = 0.0
  var lack = 0

  var moveStucked = 0

  var stopWhen: Int = STOP_WHEN_INSUFFICENT

  /*
   * Plan situation.
   */
  def insufficient = rc < rcPi
  def stucked = !satisfied && moveStucked > lambda
  def stagnated = !satisfied && wellfareStucked > lambda

  def satisfied : Boolean = ecog.satisfied

  def stopWenStucked { stopWhen = STOP_WHEN_STUCKED }
  def stopWenInsufficient { stopWhen = STOP_WHEN_INSUFFICENT }
  def isStopWhenInsufficient = stopWhen == STOP_WHEN_INSUFFICENT
  def isStopWhenStucked = !isStopWhenInsufficient

  def plan(sense: WorldSense) {
    info(sense, "Plan", plan.toString())
    
    /*
     * An agent should generate a new plan
     * only if needed.
     */
    plan match {
      case NullPlan() => {
        plan = ecog.think(sense)
      }
      case p: SingletonPlan => true
      case _ => {
        plan = ecog.think(sense)
      }
    }    
  }
  
  def act(sense: WorldSense) {
    throw new UnsupportedOperationException()
  }

  protected def doActPhy(sense: WorldSense): Boolean = {
    if (!plan.action.isPhy) return false
    val moved = body.act(plan.action.asInstanceOf[ActPhy])

    /*
     * Tests stucked.
    */
    if (!moved) {
      moveStucked += 1
    } else {
      moveStucked = 0
    }

    return moved
  }

  protected def doActSoc(sense: WorldSense): Boolean = {
    if (plan.finished || !plan.action.isSoc) return false
    /*
     * The environment may have changed.
     */
    val nrcPi = ecog.mapit(sense, plan)
    if (nrcPi != rcPi) rcPi = nrcPi
    if (this.esoc.u == 1) return true
    val coligated = esoc.act(sense, plan)

    val currentWellfare = body.soc.wellfare
    val currentLack = body.soc.lack
    if (lack != currentLack) {
//    if (wellfare != currentWellfare) {
      wellfare = currentWellfare
      lack = currentLack
      wellfareStucked = 0
    } else {
      wellfareStucked += 1
    }

    return coligated
  }

  def onReplan(plan: Plan) {
    moveStucked = 0
    wellfareStucked = 0
    
    body.ag.qtdReplan += 1
  }

}

trait PlanOnceActAllBehaviour extends PlanBehaviour {
  
  override def act(sense: WorldSense) {
    debug(sense, "Act", plan.toString())
    
    /*
     * An agent should generate a new plan
     * only if needed.
     */
    plan match {
      case NullPlan() => {
        plan = ecog.think(sense)
      }
      case p: SingletonPlan => true
      case _ => {
        val nplan = ecog.think(sense)
        onReplan(merge(nplan, plan))
      }
    }

    /* 
     * The think process can generate a NullPlan
     * if the target has already been reached.
     */
    if (plan.isNull) return
    if (doActPhy(sense)) plan.next
    if (plan.action.isSoc) {
      doActSoc(sense)
      plan.next
    }

    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }

  def merge(plan1: Plan, plan2: Plan): Plan = {
    return plan1
  }

}

/**
 * PlanCompleteActAll
 */
trait PlanCompleteActAllBehaviour extends PlanBehaviour {

  override def act(sense: WorldSense) {
    info(sense, "Act", plan.toString())    
    
    /*
     * An agent should generate a new plan
     * only if needed.
     */
    plan match {
      case NullPlan() => {
        plan = ecog.think(sense)
        onReplan(plan)
      }
      case p: SingletonPlan => true
      case _ => {
        plan = ecog.think(sense)
        onReplan(plan)
      }
    }

    /* 
     * The think process can generate a NullPlan
     * if the target has already been reached.
     */
    if (plan.isNull) return
    if (doActPhy(sense)) plan.next
    if (doActSoc(sense) && !stagnated) plan.next
    else if (plan.action.isSoc && isStopWhenStucked && stagnated) plan.next

  }

  override protected def doActPhy(sense: WorldSense): Boolean = {
    if (isStopWhenInsufficient && sense.ag.u == 1) return super.doActPhy(sense)
    //    else if (isStopWhenStucked) return super.doActPhy(sense)
    //    else return false
    else return super.doActPhy(sense)
  }

  override protected def doActSoc(sense: WorldSense): Boolean = {
    if (plan.finished || !plan.action.isSoc) return false
    /*
     * Verifies if social iteration continues.
     */
    if (stagnated) {
      plan.next
      return false
    }

    return super.doActSoc(sense)
  }

}

/**
 * PlanCompleteActReplan
 */
trait PlanCompleteActReplanBehaviour extends PlanCompleteActAllBehaviour {
  var justReplan = false
  override def onReplan(plan: Plan) {
    justReplan = true
    body.ag.qtdReplan += 1    
  }

  override def act(sense: WorldSense) {
    info(sense, "Act", plan.toString())

    if (justReplan) justReplan = false
    /*
     * Replan
     */
    if (stucked) {
      ecog.punish(plan)
      plan = ecog.think(sense)

      if (!plan.isNull && !plan.action.isSoc) plan.add(ActSoc())

      onReplan(plan)
    } else if (stagnated) {
      moveStucked = 0
      wellfareStucked = 0
    }

    super.act(sense)
  }

}

trait PlanWhenNeededActBehaviour extends PlanCompleteActAllBehaviour {
  override def act(sense: WorldSense) {
    if (plan.finished) plan = NullPlan()
    /*
     * An agent should generate a new plan
     * only if needed.
     */
    plan match {
      case NullPlan() => plan = ecog.think(sense)
      case p: SingletonPlan => true
      case _ =>
        plan = ecog.think(sense)
    }

    /* 
     * The think process can generate a NullPlan
     * if the target has already been reached.
     */
    if (plan.isNull) return

    val pi = ecog.mapit(sense, plan.remaining)
    if (rc < pi && plan.action.isPhy && stucked) {
      plan.addCurrent(ActSoc())
      wellfareStucked = 0
      moveStucked = 0
    }

    if (doActPhy(sense)) plan.next
    if (doActSoc(sense)) plan.next

    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }
}
