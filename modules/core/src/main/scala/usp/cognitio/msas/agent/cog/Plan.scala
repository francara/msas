package usp.cognitio.msas.agent.cog
import usp.cognitio.msas.agent.Act
import usp.cognitio.msas.agent.ActPhy

class Plan(var acts: List[Act]) {
  /** Current action. */
  var index: Int = 0
  
  def add(act:Act) = acts = acts ::: act :: Nil
  def addFirst(act:Act) = acts = act :: acts
  
  def next : Act = acts(index)
  
}

object Plan {
  def apply(acts:List[Act]) = new Plan(acts)
  
  implicit def actsToPlan(acts: List[Act]) : Plan = new Plan(acts)
  implicit def xysToAct(xys: List[(Int,Int)]) : Plan = new Plan(xys)
  implicit def XYToAct(xy: (Int,Int)) : ActPhy = ActPhy(xy)
  implicit def XYsToAct(xys: List[(Int,Int)]) : List[ActPhy] = xys.map(ActPhy(_))
}