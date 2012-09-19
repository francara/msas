package usp.cognitio.msas.agent
import usp.cognitio.msas.Rc
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.soc.Socialization

trait Player {
  def rcPi: Rc
  def rc: Rc
  def coalition: Coalition

  def u: Double
  def u(al: Rc): Double

  def consume(rcCoal: Rc, q: Int): Rc

}

case class SocializationPlayer(ag: Socialization) extends Player {
  def rcPi: Rc = ag.rcPi
  def rc: Rc = ag.rc
  def coalition: Coalition = ag.coalition

  def u: Double = ag.u
  def u(al: Rc): Double = ag.u(al)

  def consume(rcCoal: Rc, q: Int): Rc = ag.consume(rcCoal, q)

}