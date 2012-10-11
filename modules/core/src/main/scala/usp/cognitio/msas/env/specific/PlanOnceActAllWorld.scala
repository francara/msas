package usp.cognitio.msas.env.specific
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.env.specific.PlanOnceActAllBehaviour
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag
import org.apache.commons.math3.random.RandomDataImpl

class PlanOnceActAllWorld(val N: Int, private val _r: Int) extends GridWorld(_r) {
  val rands : Array[RandomDataImpl] = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
  var randsCell: Array[RandomDataImpl] = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
  var randCell = new RandomDataImpl()
  val randPos = new RandomDataImpl()
  
  def mean : Double = R/2
  def sigma : Double = mean/2
  def kmean = 0.1 * mean
  def ksigma = kmean / 2
  
  /*
   * Initialize cell resources.
   */
  for (x <- 0 to R) for (y <- 0 to R) randRcCell(x, y)

  override def randRcCell(x: Int, y: Int): Rc = {
      if (randCell == null) randCell = new RandomDataImpl()
      if (randsCell == null) randsCell = Array.tabulate(Rc.DIM)(k => new RandomDataImpl())
      val k = randCell.nextInt(0, Rc.DIM - 1)
      Rc((0 until Rc.DIM).toList.map(v => if (v == k) randsCell(k).nextGaussian(kmean, ksigma).asInstanceOf[Int] else 0))
  }
  override def randPosition(ag: MsasAg): (Point, Point) = (Point(randPos.nextInt(0,R-1), randPos.nextInt(0,R-1)), Point(R/2, R/2))

  override def randRcAg(ag: Ag) : Rc = {
    var rc_ks : List[Int] = Nil
    for (k <- 0 to Rc.DIM-1) {
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
    ags.foreach(ag => {
      val sense = this.sense(ag)
      ag.act(sense)
    })
  }

  def populate() {
    for (i <- 1 to N) {
      val ag = new MsasAg(i, Rc()) with PlanOnceActAllBehaviour
      ag.init(this, this)
      enter(ag)
    }
  }
  
  def satisfied() : Boolean = !ags.exists(!_.satisfied)

}