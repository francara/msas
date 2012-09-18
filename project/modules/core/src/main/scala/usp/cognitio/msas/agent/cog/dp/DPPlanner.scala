package usp.cognitio.msas.agent.cog.dp
import usp.cognitio.msas.agent.cog.dp.DPPlanner._
import usp.cognitio.math.alg._
import usp.cognitio.math.alg.Point._
import usp.cognitio.msas.kernel.MsasMathObject
import scala.collection._
import usp.cognitio.msas.agent.cog.plan.Space

class DPPlanner(space: Space) extends MsasMathObject {
  def N = space.N
  class DPPlannerCost(planner: DPPlanner, val p: (Int, Int), val v: Option[Double]) {
    def :=(v: Double) { planner.cCost(p._1)(p._2) = Some(v) }
  }

  private val cCost: Array[Array[Option[Double]]] = Array.tabulate(space.N, space.N)((x, y) => None)
  private val cPenalty: Array[Array[Boolean]] = Array.tabulate(space.N, space.N)((x, y) => false)

  def cost(x: Int)(y: Int): DPPlannerCost = cost(x, y)
  def cost(p: (Int, Int)): DPPlannerCost = new DPPlannerCost(this, (p._1, p._2), cCost(p._1)(p._2))
  def hasPenalty(x: Int, y: Int): Boolean = hasPenalty((x, y))
  def hasPenalty(p: (Int, Int)): Boolean = cPenalty(p._1)(p._2)
  def punish(x: Int, y: Int): DPPlanner = punish((x, y))
  def punish(p: (Int, Int)) : DPPlanner = { cPenalty(p._1)(p._2) = true; this}

  /* Inicializa custo. */
  C(0, 0)

  def plan(cell : (Int,Int)) {
    cCostApply((x,y) => {None})   // applies None to all cCost elements.    
    C(cell)
  }
  
  /**
   * C function: COST FUNCTION
   * -------------------------
   * The main planning function.
   * Calculates the cost of each cell.
   */
  private def C(cell: (Int, Int)): Double = {
    val x = cell._1
    val y = cell._2
    cCost(x)(y) match {
      case Some(d) => return d
      case None =>
        if (cell == space.target) {
          cCost(x)(y) = Some(0.0)
          return 0.0
        }

        var neighs: List[(Int, Int)] = List()
        var cNeighs = mutable.Map.empty[(Int,Int), Double]
        /*
         * The neighs are built following the gradient. 
         */
        if (x + 1 < space.N) neighs = (x + 1, y) :: neighs
        if (y + 1 < space.N) neighs = (x, y + 1) :: neighs
        // Neighs cost
        neighs.map(n => (n,C(n))).foreach(el => cNeighs(el._1) = el._2)

        if (neighs.isEmpty) cCost(x)(y) = Some(space.state(x)(y))
        else
          /*
           * cost :=    gradient,        pointer to the target
           *         +  min{C(neighs)},  neigh with the lower cost
           *         +  penalty,         punishment for lack of resources
           */
          cCost(x)(y) = Some(round(
            space.state(x)(y)
//              + neighs.map(C(_)).min
              + cPenalty(x)(y)))
        return cCost(x)(y).get
    } // matches cCost
  } // function C

  protected def cCostApply(f: (Int,Int) => Option[Double]) : Unit = {
    for (i <- 0 to space.N-1) for (j <- 0 to space.N-1) cCost(i)(j) = f(i,j)
  }
  
  /**
   * PATH
   * ----
   * Builds a path to the target.
   * A path is a list o coordinates.
   */
  def path(x: Int, y: Int): List[(Int, Int)] = path((x, y))
  def path(source: (Int, Int)): List[(Int, Int)] = {
    if (source == space.target.xy) return List(space.target)

    val (sx, sy) = source
    val (tx, ty) = space.target.xy

    val neigs = neighs(sx, sy)
    val nord = neigs.map(n => cCost(n._1)(n._2).get).zipWithIndex
      .sort(_._1 < _._1)
    // Takes the neigh cell with least cost.
    val n: Int = nord(0)._2
    return source :: path(neigs(n))
  }

  /**
   * UNPATH
   * ------
   * Increments the path's cost to  obly a branch.
   *
   * The branch affects the last cell in the path. This cell receives
   * a penalty to obbly the next planning avoid her.
   */
  def unpath(p: List[(Int, Int)]): Unit = {
    if (p.last == space.target.xy) punish(p.reverse.tail.head)
    else punish(p.last)
    // Resets cost.
    cCostApply((x,y) => {None})   // applies None to all cCost elements.
    C(0, 0)
  }
  
  private def neighs(x: Int, y: Int): List[(Int, Int)] = {
    var cells: List[(Int, Int)] = List()
    if (x - 1 >= 0) cells = (x - 1, y) :: cells
    if (y - 1 >= 0) cells = (x, y - 1) :: cells
    if (x + 1 < space.N) cells = (x + 1, y) :: cells
    if (y + 1 < space.N) cells = (x, y + 1) :: cells
    return cells
  }

  override def toString: String = {
    var buffer: StringBuffer = new StringBuffer
    for (y <- N-1 to 0 by -1) {
      buffer.append("\n")
      for (x <- 0 to N-1) {
        buffer.append("%3.2f".format(cCost(x)(y).get).toString).append(" ")
      }
    }

    return buffer.toString()
  }
}

object DPPlanner {
  def apply(N:Int) : DPPlanner = new DPPlanner(new Space(N))
  implicit def BooleanToDouble(b: Boolean): Double = if (b) 1000.0 else 0
  implicit def DPPlannerCostToDouble(dpc: DPPlanner#DPPlannerCost): Double = dpc.v.getOrElse(0.0)
}

