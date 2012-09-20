package usp.cognitio.msas.env
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.math.alg.Point

trait WorldPhy {
  val N:Int
  val rcs: Array[Array[Rc]] = Array.tabulate(N, N)((x, y) => Rc())
  
  def ags : Array[MsasAg]
  def position(ag: Ag) : Point
  def move(ag:Ag, pos: Point) : Boolean  
  def sense(ag:MsasAg) : WorldSense
}