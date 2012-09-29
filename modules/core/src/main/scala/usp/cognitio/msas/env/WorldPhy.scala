package usp.cognitio.msas.env
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.Rc

trait WorldPhy {
  val R:Int
  val rcs: Array[Array[Rc]]
  
  def randRcCell(x: Int, y: Int) : Rc
  def randRcAg(ag: Ag) : Rc
  def randPosition(ag: MsasAg) : (Point, Point)
  
  def ags : Array[MsasAg]
  def position(ag: Ag) : Point
  def move(ag:Ag, pos: Point) : Boolean
  def sense(ag:MsasAg) : WorldSense
}