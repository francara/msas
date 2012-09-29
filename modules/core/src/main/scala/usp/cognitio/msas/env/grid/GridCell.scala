package usp.cognitio.msas.env.grid
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.env.Direction
import scala.util.Random
import org.apache.commons.lang.builder.HashCodeBuilder
import usp.cognitio.msas.Rc
import usp.cognitio.math.alg.Point

class GridCell(val x : Int, val y : Int, grid : GridWorld) {
  /**
   * Agents occupying the cell.
   */
  var ags : List[Ag] = List()
  
//  val rc : Rc = {
//    val coord = Random.nextInt(Rc.DIM)
//    var resources : List[Int] = List()
//    for (i <- 0 to Rc.DIM-1) 
//      if (i == coord) resources = 1 :: resources
//      else resources = 0 :: resources
//    new Rc(resources)
//  }

  def rc : Rc = grid.rcs(x)(y)
  
  def in(ag : Ag) : GridCell = { ags = ag :: ags; this }
  def out(ag : Ag) : GridCell = { ags = ags.remove(_ == ag); this }
  def who : List[Ag] = ags
  def contains(ag : Ag) : Boolean = ags.exists(_ == ag)
  def next(d : Direction.Value) : GridCell = grid.next(x,y,d)
  def neigh(p: Point) : GridCell = 
    if (Point(x,y).neighs.contains(p)) grid.cells(p.x)(p.y)
    else this
  
  def point = Point(x,y)
  
  def clone(rcClone : Rc) : GridCell = {
    val other = new GridCell(x,y,grid) {
      override val rc = rcClone
    }
    other.ags = this.ags
    return other
  }
  
  override def toString = 
    "[" + "(" + x.toString + "," + y + ")" + "{" + rc + "} => " + ags.toString + "]"
  override def equals(any : Any) : Boolean =
    any match {
    case other : GridCell => canEquals(any) && x == other.x && y == other.y
    case _ => false
  }
  def canEquals(any : Any) : Boolean = any.isInstanceOf[GridCell]
  override def hashCode : Int = new HashCodeBuilder().append(x).append(y).toHashCode() 
}

trait SingleOccupy extends GridCell {
  abstract override def in(ag : Ag) : GridCell = {
    if (super.who.length != 0) throw new UnsupportedOperationException()
    super.in(ag)
  }
}
