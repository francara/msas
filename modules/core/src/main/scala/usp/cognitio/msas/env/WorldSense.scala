package usp.cognitio.msas.env
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.MsasAg

case class WorldSense(
    val N:Int, val position:Point,
    private val wphy: WorldPhy,
    private val wsoc: WorldSoc) {
  
  def rcs: Array[Array[Rc]] = wphy.rcs
  val neighs : Array[MsasAg] = Array.empty[MsasAg]
  def coal : Map[MsasAg,Coalition] = wsoc.coal
}