package usp.cognitio.msas.agent.cog
import usp.cognitio.msas.agent.Act
import usp.cognitio.msas.agent.ActPhy
import usp.cognitio.msas.agent.ActSoc

case class Plan(var acts: List[Act]) {
  /** Current action. */
  var index: Int = 0
  
  def add(act:Act) = acts = acts ::: act :: Nil
  def addFirst(act:Act) = acts = act :: acts
  def addCurrent(act: Act) = {acts = acts.take(index) ::: act :: acts.drop(index)}
  
  def next : Boolean = doNext
  protected def doNext : Boolean = {
    if (index < acts.size-1) {
      index += 1; 
      true
    } else false
  }
  def action : Act = acts(index)
  def isNull = false
  
  def finished : Boolean = index >= acts.size - 1
  
  def remaining : Plan = Plan(acts.drop(index))
  
  def path : Path = {
    Path(acts.filter(_.isInstanceOf[ActPhy]).map(_.asInstanceOf[ActPhy].target))
  }
  
  override def toString() : String = "index: " + index + ", " + path.toString()
}

case class NullPlan extends Plan(Nil) {
  override def isNull = true
}

case class SingletonPlan(val plan: Plan) extends Plan(plan.acts)

object Plan {  
  implicit def actsToPlan(acts: List[Act]) : Plan = new Plan(acts)
  implicit def xysToAct(xys: List[(Int,Int)]) : Plan = new Plan(xys)
  implicit def XYToAct(xy: (Int,Int)) : ActPhy = ActPhy(xy)
  implicit def XYsToAct(xys: List[(Int,Int)]) : List[ActPhy] = xys.map(ActPhy(_))
}