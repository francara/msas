package usp.cognitio.msas.agent.cog.plan
import usp.cognitio.math.alg.Point._
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.cog.plan.Space._

class Space(val N: Int) {
  val state: Array[Array[Double]] = Array.tabulate(N, N)((x, y) => 1.0)
  val cost: Array[Array[Double]] = Array.tabulate(N, N)((x, y) => 0.00)
  
  /** Célula objetivo. */
  var target: Point = ((N / 2).round, (N / 2).round)
  
  def punish(cell: Point) = cost(cell.x)(cell.y) += PLAMBDA
  def punish(cell: Point, plambda: Double) = cost(cell.x)(cell.y) += plambda
  
  def edge(p1: Point, p2: Point) : Double = cost(p2.x)(p2.y)
  
  /*
   * Distribui um custo aleatório nas células.
   */
  for (i <- 0 to N - 1)
    for (j <- 0 to N - 1) {
      state(i)(j) = Point(i, j).dist(target)
    }

  def apply(cell: (Int,Int)) = state(cell._1)(cell._2)
  
  def neighboors(cell : Point) : List[Point] = {
    var (x,y) = cell.xy
    var cells: List[Point] = List()
    if (x - 1 >= 0) cells = (x - 1, y) :: cells
    if (y - 1 >= 0) cells = (x, y - 1) :: cells
    if (x + 1 < N) cells = (x + 1, y) :: cells
    if (y + 1 < N) cells = (x, y + 1) :: cells
    return cells
  }
  
  override def toString: String = {
    var buffer: StringBuffer = new StringBuffer
    for (y <- N-1 to 0 by -1) {
      buffer.append("\n")
      for (x <- 0 to N-1) {
        buffer.append("%2.2f".format(state(x)(y) + cost(x)(y)).toString).append(" ")
      }
    }

    return buffer.toString()
  }

  private def round(d: Double) = BigDecimal(d).setScale(2, 
      BigDecimal.RoundingMode.FLOOR).toDouble

}

object Space {
  var PLAMBDA = 10.00
  
  def apply() = new Space(Point.MAX)
}