package usp.cognitio.msas.env
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg

trait WorldPhy {
  val N:Int
  val rcs: Array[Array[Rc]] = Array.tabulate(N, N)((x, y) => Rc())
  
  def ags : Array[MsasAg]
  def move(ag:Ag, direcion: Int) : Boolean = true
  
  def sense(ag:MsasAg) : WorldSense
}