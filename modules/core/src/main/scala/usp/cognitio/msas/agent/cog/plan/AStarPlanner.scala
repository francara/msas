package usp.cognitio.msas.agent.cog.plan
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.cog.Path
import scala.collection._
import java.util.Comparator

case class AStarPlanner(val edge: ((Point,Point) => Double), source:Point, target: Point) {
  
  var star = mutable.Map.empty[Point, NodeStar]
  case class NodeStar(var point:Point) extends Comparable[NodeStar] {
    var _g:Double = 0
    protected var _f:Double = 0 
    protected var _h:Double = 0
    protected var _previous : Point = null
    def f(d:Double) = {_f=d; this}
    def g(d:Double) = {_g=d; this}
    def h(d:Double) = {_h=d; this}
    def prev(p:Point) = {_previous=p; this}
    def g = _g;  def f = _f; def h = _h; def prev = _previous

    star.remove(point)
    star(point) = this

    def cast : OpenNodeStar = this.asInstanceOf[OpenNodeStar]
    override def compareTo(other: NodeStar) : Int = 
      if (f<other.f) -1 else if (f>other.f) 1 
      else if (point.x<other.point.x) 1 else if (point.x>other.point.x) -1 else 0
    override def equals(o:Any) : Boolean =  if (!o.isInstanceOf[NodeStar]) false else this.point == o.asInstanceOf[NodeStar].point
    override def hashCode : Int = this.point.hashCode()
    override def toString : String =  "Point[" + point + "]" + ", f:" + f + ", g:" + g + ", h:" + h
    
  }
  case class OpenNodeStar(_point:Point) extends NodeStar(_point) {
    opened += point
    closed -= point
    def enqueue = {queue.add(this); this}
    AStarListeners.open(point, target)
  } 
  case class CloseNodeStar(op : OpenNodeStar) extends NodeStar(op.point) {
    g(op.g); f(op.f); prev(op.prev)
    closed += point
    opened -= point    
    AStarListeners.close(point, target)
  } 

  var opened = mutable.Set.empty[Point]
  var closed = mutable.Set.empty[Point]
  var queue = new java.util.PriorityQueue[OpenNodeStar]()
  
  def * : Path = {
    OpenNodeStar(source).g(0).h(source.dist(target)).f(source.dist(target))
      .cast.enqueue
    while (!opened.isEmpty) {
      val who = queue.poll()
      if (who.point == target) return path
      
      CloseNodeStar(who)
      who.point.neighs.filter(!closed.contains(_))
        .foreach( neigh => {
          val g_score = who.g + edge(who.point, neigh)
          
          var nstar = if (star.contains(neigh)) star(neigh)
          else 
            OpenNodeStar(neigh)
              .g(g_score)
              .h(neigh.dist(target))
              .f(g_score + neigh.dist(target))
              .prev(who.point)
              .cast.enqueue
          
          if (g_score < star(neigh).g) 
            nstar.g(g_score).f(g_score + neigh.dist(target)).prev(who.point)
          
        })
    }
    return null 
  }

  private def path : Path = {
    var points : List[Point] = Nil
    var next = target
    while (next != null) {
      points = next :: points
      next = star(next).prev
    }
    
    return Path(points)
  }
  
}

object AStarListeners {
  var open : (Point, Point) =>  Unit = {(a,b) => null}
  var close : (Point, Point) =>  Unit = {(a,b) => null}
}
