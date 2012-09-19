package usp.cognitio.msas.agent
import usp.cognitio.math.alg.Point

case class Act(var executed:Boolean = false)
case class ActSoc extends Act { override def toString = "COAL"}
case class ActPhy(val target:Point) extends Act {
  override def toString = "MV: " + target.toString
}