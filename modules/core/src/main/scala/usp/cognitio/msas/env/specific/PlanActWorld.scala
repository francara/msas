package usp.cognitio.msas.env.specific
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.env.spy.SpyFile
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.math.alg.Point
import org.apache.log4j.Logger
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.Plan
import scala.util.Random

class PlanActWorld(val N: Int, private val _r: Int, var kmeanScale: Double = 0.1) extends GridWorld(_r) {
  val logger = Logger.getLogger(getClass().getName());

  Point.DIAGONAL = true
  val spy = SpyFile()

  val rands: Array[RandomDataImpl] = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
  var randsCell: Array[RandomDataImpl] = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
  var randCell = new RandomDataImpl()
  val randPos = new RandomDataImpl()

  
  def mean: Double = R / 1.5
  def sigma: Double = mean / 4
  def kmean = kmeanScale * mean
  def ksigma = kmean / 4

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
    val els = (0 to Rc.DIM - 1).map(k => randsCell(k).nextGaussian(kmean, ksigma).asInstanceOf[Int]).toList
    Rc(els)
  }
  override def randPosition(ag: MsasAg): (Point, Point) = (Point(randPos.nextInt(0, R - 1), randPos.nextInt(0, R - 1)), Point(R / 2, R / 2))

  val r_lack = new Random()
  val LACK = 0.5
  override def randRcAg(ag: Ag): Rc = {
    def lack(ks: List[Int]) : List[Int] = {
      var lacked = false
      var n_ks : List[Int] = Nil
      for (k <- 0 to ks.size-1) {
        if (r_lack.nextDouble() < LACK && !lacked) {
          n_ks = 0 ::n_ks
          lacked = true
        } else n_ks = ks(k) ::n_ks
      }
      n_ks.reverse
    }
    
    var rc_ks: List[Int] = Nil
    for (k <- 0 to Rc.DIM - 1) {
      var distrib = rands(k).nextGaussian(mean, sigma).asInstanceOf[Int]
      if (distrib < 0) distrib = distrib * -1
      rc_ks = distrib :: rc_ks
    }

    Rc(lack(rc_ks))
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

  def plan() {
    ags.foreach(ag => {
      val sense = this.sense(ag)
      ag.plan(sense)
    })
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
    currentIteration = 0
    for (i <- 1 to N) {
      /*
       * BEHAVIOUR
       * ---------
       */
      val ag = create(i)
      ag.stopWenStucked

      ag.init(this, this)
      enter(ag)
    }
  }

  def satisfied(): Boolean = !ags.exists(!_.satisfied)
  def done(): Boolean = ags.filter(!_.satisfied).filter(ag => !ag.stagnated && !ag.stucked).size == 0
  def create(id: Int): PlanAg = new PlanAg(this, id, Rc()) with PlanOnceActAllBehaviour
  
  def code : String = ""
}

case class PlanAg(world: PlanActWorld, _i: Int, _rc: Rc) extends MsasAg(_i, _rc) {
  override def act(sense: WorldSense) {
    if (plan.isNull) return super.act(sense)

    if (world.phySem && plan.action.isPhy) {
      return
    } else if (world.socialSem && plan.action.isSoc) {
      //        logger.debug(socializing(this))
      return
    } else {
      //        logger.debug(situation(this))
      return super.act(sense)
    }
  }
  override def onReplan(plan: Plan) {
    super.onReplan(plan)
    //      logger.debug(replanning(this))
  }
}