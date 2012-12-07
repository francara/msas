package usp.cognitio.msas.env.specific
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.env.specific.PlanOnceActAllBehaviour
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.msas.env.spy.SpyFile
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.env.spy.PlanSpy
import org.apache.log4j.Logger
import usp.cognitio.msas.agent.cog.Plan

/**
 * World with agents that plan once and act.
 *
 * Agent behavior is configures (PlanBehaviour).
 * Spy to save a file with actions (PlanSpy).
 */
class PlanOnceActAllWorld(val N: Int, private val _r: Int) extends GridWorld(_r) with PlanSpy {
  override val logger = Logger.getLogger(getClass().getName());

  Point.DIAGONAL = true
  val spy = SpyFile()

  val rands: Array[RandomDataImpl] = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
  var randsCell: Array[RandomDataImpl] = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
  var randCell = new RandomDataImpl()
  val randPos = new RandomDataImpl()

  def mean: Double = R / 2
  def sigma: Double = mean / 2
  def kmean = 0.1 * mean
  def ksigma = kmean / 2

  /*
   * Semaphores
   */
  var socialSem: Boolean = false
  var phySem: Boolean = false

  /*
   * Initialize cell resources.
   */
  for (x <- 0 to R) for (y <- 0 to R) randRcCell(x, y)

  override def randRcCell(x: Int, y: Int): Rc = {
    if (randCell == null) randCell = new RandomDataImpl()
    if (randsCell == null) randsCell = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
//    val k = randCell.nextInt(0, Rc.DIM - 1)
//    Rc((0 until Rc.DIM).toList.map(v => if (v == k) randsCell(k).nextGaussian(kmean, ksigma).asInstanceOf[Int] else 0))
    val els = (0 to Rc.DIM-1).map(k => randsCell(k).nextGaussian(kmean, ksigma).asInstanceOf[Int]).toList 
    Rc(els)
  }
  override def randPosition(ag: MsasAg): (Point, Point) = (Point(randPos.nextInt(0, R - 1), randPos.nextInt(0, R - 1)), Point(R / 2, R / 2))

  override def randRcAg(ag: Ag): Rc = {
    var rc_ks: List[Int] = Nil
    for (k <- 0 to Rc.DIM - 1) {
      var distrib = rands(k).nextGaussian(mean, sigma).asInstanceOf[Int]
      if (distrib < 0) distrib = distrib * -1
      rc_ks = distrib :: rc_ks
    }

    Rc(rc_ks)
  }

  /**
   * Randomize position, source and target.
   */
  def enter(ag: MsasAg): GridWorld = {
    val (start, target) = this.randPosition(ag)
    ag.target = target
    ag.rc = randRcAg(ag)
    enter(ag, start.x, start.y)
  }

  def act() {
    currentIteration += 1
    ags.foreach(ag => {
      val sense = this.sense(ag)
      if (!ag.satisfied) ag.act(sense)
    })
  }

  def populate() {
    def situation(ag: MsasAg) =
      "[" + ag + "] SITUATION: " +
        "{insufficient: " + ag.insufficient + ", " +
        "stucked: " + ag.stucked + ", " +
        "stagnated: " + ag.stagnated + "}"
    def socializing(ag: MsasAg) = "[" + ag + "] Socializing"
    def replanning(ag: MsasAg) = "[" + ag + "] Replanning"

    this.clear()
    for (i <- 1 to N) {
      /*
       * BEHAVIOUR
       * ---------
       */
      case class PlanAg(world: PlanOnceActAllWorld, _i: Int, _rc: Rc) extends MsasAg(_i, _rc)
      val ag = new PlanAg(this, i, Rc()) with PlanOnceActAllBehaviour {
        override def act(sense: WorldSense) {
          if (plan.isNull) return super.act(sense)

          if (world.phySem && plan.action.isPhy) {
            return
          } else if (world.socialSem && plan.action.isSoc) {
            logger.debug(socializing(this))
            return
          } else {
            logger.debug(situation(this))
            return super.act(sense)
          }
        }
        override def onReplan(plan: Plan) {
          super.onReplan(plan)
          logger.debug(replanning(this))
        }
      }

      ag.stopWenStucked

      ag.init(this, this)
      enter(ag)
    }
  }

  def satisfied(): Boolean = !ags.exists(!_.satisfied)

}