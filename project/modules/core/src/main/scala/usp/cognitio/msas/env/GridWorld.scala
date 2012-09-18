package usp.cognitio.msas.env
import scala.collection.mutable.Map
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.grid.GridCell
import Direction.North
import Direction.South
import Direction.East
import Direction.West
import usp.cognitio.msas.Rc

/**
 * Represents the environment in a matrix N x N.
 */
class GridWorld(val N : Int) {
  val cells : Array[Array[GridCell]] = Array.tabulate(N,N)((x,y) => new GridCell(x,y,this))
  val where : Map[Ag,GridCell] = scala.collection.mutable.Map[Ag,GridCell]()
  
  def apply(x:Int, y:Int) : GridCell = cells(x)(y)
  
  def enter(ag : Ag, x:Int, y:Int) : GridWorld = {
    where(ag) = cells(x)(y).in(ag)
    return this
  }
  
  def whereIs(ag : Ag) : GridCell = where(ag)
  def moveTo(ag : Ag, d : Direction.Value) : GridCell = {
    val cell = where(ag).out(ag).next(d).in(ag)
    where(ag) = cell
    return cell
  }
  
  def next(x : Int, y : Int, d : Direction.Value) : GridCell = {
    val deltaX = if (d == East) +1 else if (d == West) -1 else 0
    val deltaY = if (d == North) +1 else if (d == South) -1 else 0
    
    val nx = x + deltaX
    val ny = y + deltaY
    
    if (nx >= 0 && nx < N && ny >= 0 && ny < N) return cells(nx)(ny)
    else return cells(x)(y)
  }
  
  def putRc(x:Int, y:Int, rc:Rc) {
    cells(x)(y) = cells(x)(y).clone(rc)
    cells(x)(y).ags.foreach( ag => where(ag) = cells(x)(y))
  }
  
  /**
   * Total of resources to complete the path.
   * @return One resource vector representing the path.
   */
  def toRc(path : List[(Int,Int)]) : Rc = Rc.total(for(p <- path) yield cells(p._1)(p._2).rc)
}

object Direction extends Enumeration {
  val North, East, South, West = Value
}
