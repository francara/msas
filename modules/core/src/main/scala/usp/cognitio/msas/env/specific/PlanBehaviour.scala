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

trait PlanBehaviour {
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

  var moveStucked = 0
  def stucked = moveStucked > lambda

  def act(sense: WorldSense) {
    throw new UnsupportedOperationException()
  }

  protected def doActPhy(sense: WorldSense): Unit = {
    if (plan.action.isPhy) {
      val moved = body.act(plan.action.asInstanceOf[ActPhy])

      /*
       * Tests stucked.
       */
      if (!moved) {
        moveStucked += 1
      } else {
        moveStucked = 0
      }

      if (moved) plan.next
      return
    }
  }

  /**
   * Stops social iteration.
   */
  protected def stop(): Boolean = {
    if (wellfareStucked > lambda) return true

    val currentWellfare = body.soc.wellfare
    if (wellfare != currentWellfare) {
      wellfare = currentWellfare
      wellfareStucked = 0
    } else {
      wellfareStucked += 1
    }
    false
  }
}

trait PlanOnceActAllBehaviour extends PlanBehaviour {
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
        val nplan = ecog.think(sense)
        merge(nplan, plan)
    }

    /* 
     * The think process can generate a NullPlan
     * if the target has already been reached.
     */
    if (plan.isNull) return
    doActPhy(sense)
    doActSoc(sense)

    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }

  def merge(plan1: Plan, plan2: Plan): Plan = {
    return plan1
  }

  protected def doActSoc(sense: WorldSense): Unit = {
    if (plan.finished || !plan.action.isSoc) return
    /*
     * The environment may have changed.
     */
    val nrcPi = ecog.mapit(sense, plan)
    if (nrcPi == rcPi) return

    rcPi = nrcPi
    if (plan.action.isSoc) esoc.act(sense, plan)
    plan.next
  }
}

trait PlanCompleteActAllBehaviour extends PlanOnceActAllBehaviour {
  var STOP_WHEN_STUCKED = 0
  var STOP_WHEN_INSUFFICENT = 1
  
  var stopWhen : Int = STOP_WHEN_INSUFFICENT 
  
  def stopWenStucked {stopWhen = STOP_WHEN_STUCKED}
  def stopWenInsufficient {stopWhen = STOP_WHEN_INSUFFICENT}
  def isStopWhenInsufficient = stopWhen == STOP_WHEN_INSUFFICENT
  def isStopWhenStucked = !isStopWhenInsufficient
  
  override protected def doActPhy(sense: WorldSense): Unit = {
    if (isStopWhenInsufficient && sense.ag.u == 1) super.doActPhy(sense)
    else if (isStopWhenStucked) super.doActPhy(sense)
  }
  
  override protected def doActSoc(sense: WorldSense): Unit = {
    if (plan.finished || !plan.action.isSoc) return
    /*
     * Verifies if social iteration continues.
     */
    if (stop()) {
      plan.next
      return
    }

    /*
     * The environment may have changed.
     */
    val nrcPi = ecog.mapit(sense, plan)
    rcPi = nrcPi
    if (plan.action.isSoc) esoc.act(sense, plan)
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
        val nplan = ecog.think(sense)
        merge(nplan, plan)
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

    doActPhy(sense)
    doActSoc(sense)

    /*
     * Decide the physical action based
     * on the executed social action.
     */
  }
}
