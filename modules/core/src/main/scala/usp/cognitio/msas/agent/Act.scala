package usp.cognitio.msas.agent
import usp.cognitio.math.alg.Point

case class Act(var executed: Boolean = false) {
  def isSoc = this.isInstanceOf[ActSoc]
  def isPhy = this.isInstanceOf[ActPhy]
}
case class ActSoc extends Act { override def toString = "COAL" }
case class ActPhy(val target: Point) extends Act {
  override def toString = "MV: " + target.toString
}