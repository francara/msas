package usp.cognitio.msas.env
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.coal.Coalition

case class WorldSense(val N:Int, val position:Point) {
  val rcs: Array[Array[Rc]] = Array.tabulate(N, N)((x, y) => Rc())
  val neighs : Array[Ag] = Array.empty[Ag]
  val coal : Map[Ag,Coalition] = Map.empty[Ag,Coalition]
}