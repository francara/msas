package usp.cognitio.msas.env
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.agent.cog.plan.Space

case class WorldSense (
    val N:Int, val ag: MsasAg, val position:Point,
    private val wphy: WorldPhy,
    private val wsoc: WorldSoc) {
  
  def rcs: Array[Array[Rc]] = wphy.rcs
  val neighs : Array[MsasAg] = wphy.ags.filter( _ != ag)
  def coals : Map[MsasAg,Coalition] = wsoc.coals

}