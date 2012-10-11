package usp.cognitio.msas.env
import scala.collection.mutable.Map
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.grid.GridCell
import Direction.North
import Direction.South
import Direction.East
import Direction.West
import usp.cognitio.msas.Rc
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.MsasAg
import scala.collection.mutable.ArrayBuffer

/**
 * Represents the environment in a matrix N x N.
 */
class GridWorld(val R : Int) extends WorldSoc with WorldPhy {
  Point.MAX = R
  val cells : Array[Array[GridCell]] = Array.tabulate(R,R)((x,y) => new GridCell(x,y,this))
  private var _ags : ArrayBuffer[MsasAg] = new ArrayBuffer[MsasAg]()
  val where : Map[Ag,GridCell] = scala.collection.mutable.Map[Ag,GridCell]()
  
  val rcs: Array[Array[Rc]] = Array.tabulate(R,R)((x, y) => randRcCell(x,y))
  
  def ag(i: Int) = ags.find(_.id == i).head
  def ags = _ags.toArray
  def sense(ag: MsasAg): WorldSense = WorldSense(R, ag, where(ag).point, this, this)
  
  /**
   * Randomize cell resources.
   * Define the resource needed for cell occupation.
   */
  def randRcCell(x: Int, y: Int) : Rc = {
	  Rc.nil
  }
  
  /**
   * Define agent self resources.
   */
  def randRcAg(ag: Ag) : Rc = {
    ag.rc
  }
  
  /**
   * Define agent initial position and agent target.
   */
  def randPosition(ag: MsasAg) : (Point, Point) = {
    (Point(0,0), Point(R/2, R/2))
  }

  def apply(x:Int, y:Int) : GridCell = cells(x)(y)

  def enter(ag : MsasAg, x:Int, y:Int) : GridWorld = {
    _ags += ag
    where(ag) = cells(x)(y).in(ag)
    this.coals += (ag -> ag.coalition)
    return this
  }
  
  def position(ag: Ag) = where(ag).point
  def whereIs(ag : Ag) : GridCell = where(ag)
  def moveTo(ag : Ag, d : Direction.Value) : GridCell = {
    val cell = where(ag).out(ag).next(d).in(ag)
    where(ag) = cell
    return cell
  }
  def move(ag:Ag, pos: Point) : Boolean = {
    /*
     * Verifies if the agent has got the resources.
     */
    val crc = rcs(pos.x)(pos.y)
    val arc = ag.rc
    if (crc > arc) return false
    
    val curcell = where(ag)
    val ncell = moveToNeigh(ag, pos)
    val result = if (curcell != ncell) true else false
    
    // Consume ag resource
    if (result) ag.rc = ag.rc - crc
    
    return result
  }  
  def moveToNeigh(ag: Ag, p: Point) : GridCell = {
    if (where(ag).neigh(p) == where(ag)) where(ag)
    val cell = where(ag).out(ag).neigh(p).in(ag)
    where(ag) = cell
    return cell
  }
  
  def next(x : Int, y : Int, d : Direction.Value) : GridCell = {
    val deltaX = if (d == East) +1 else if (d == West) -1 else 0
    val deltaY = if (d == North) +1 else if (d == South) -1 else 0
    
    val nx = x + deltaX
    val ny = y + deltaY
    
    if (nx >= 0 && nx < R && ny >= 0 && ny < R) return cells(nx)(ny)
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
