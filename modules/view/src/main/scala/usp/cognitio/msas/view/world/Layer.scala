package usp.cognitio.msas.view.world
import scalafx.scene.Group
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.Ag
import javafx.beans.value.ChangeListener
import scalafx.scene.canvas.Canvas
import scalafx.scene.shape.MoveTo
import scalafx.scene.shape.ArcTo
import scalafx.scene.shape.Path
import scalafx.scene.shape.Line
import scalafx.scene.shape.QuadCurve
import scala.math.abs
import scala.math.min
import scalafx.scene.paint.Color
import java.util.ArrayList

trait Layer extends Group
case class PhaseLayer(world: AgWorld) extends Layer {
  children.add(Stucked(world.cell(1,1)))
//	world.cells.foreach(_.foreach( cell => {
//	  children.add(Stucked(cell))
//	}
//	))
}

/**
 * Show a coalition in a world.
 */
case class CoalLayer(world: AgWorld, coal: Coalition) extends Layer {
  var members = scala.collection.mutable.Map.empty[Ag, Member]
  def ags = members.keys.map(ag => ag)

//  var assocs = scala.collection.mutable.Map.empty[Ag, java.util.List[Association]]
//  var pairAssocs = scala.collection.mutable.Map.empty[(Ag,Ag), Association]
//  coal.members.foreach(ag => {
//    assocs += (ag -> new ArrayList[Association]())
//    coal.members.filter(_ != ag).map((ag,_)).foreach(pair => {
//      val assoc = Association(this, world.ags(pair._1), world.ags(pair._2))
//      assocs(ag).add(assoc)
//      pairAssocs += (pair -> assoc)
//      pairAssocs += ((pair._2, pair._1) -> assoc)
//    })
//  })
  
  coal.members.foreach(ag => {
    val agent = world.agent(ag.id)
    members(ag) = Member(coal, agent)

    agent.centerX.addListener(new ChangeListener[Number] {
      override def changed(obs: javafx.beans.value.ObservableValue[_ <: Number], ovl: Number, nvl: Number) {
        members(agent.ag).centerX = nvl.doubleValue()
//        val agAssocs = assocs(agent.ag).toArray()
//        agAssocs.foreach(association => {
//        	  association.asInstanceOf[Association].moveX(agent.ag, nvl.doubleValue())
//        })
      }
    })
    agent.centerY.addListener(new ChangeListener[Number] {
      override def changed(obs: javafx.beans.value.ObservableValue[_ <: Number], ovl: Number, nvl: Number) {
        members(agent.ag).centerY = nvl.doubleValue()
//        assocs(agent.ag).toArray()
//        	.foreach(association => association.asInstanceOf[Association].moveY(agent.ag, nvl.doubleValue()))
      }
    })

    children.add(members(ag))
  })

//  override def equals(any: Any): Boolean =
//    any match {
//      case other: CoalLayer => canEquals(any) && coal == other.coal
//      case _ => false
//    }
//  def canEquals(any: Any): Boolean = any.isInstanceOf[CoalLayer]
//  override def hashCode: Int = coal.hashCode()
  
}

case class Association(val layer: CoalLayer, val source: Agent, val target: Agent) {
  def moveX(ag: Ag, x: Double) = if (ag == source) moveXSource(x) else moveXTarget(x)
  def moveY(ag: Ag, y: Double) = if (ag == source) moveYSource(y) else moveYTarget(y)

  var (x1, x2, y1, y2) = (source.x, target.x, source.y, target.y)

  val line = Line(x1, y1, x2, y2)
  val quad = QuadCurve(x1, y1, 0.0, 0.0, x2, y2)
  quad.fill = new Color(Color.WHITE.opacity(0.1))
  quad.stroke = Color.DARKBLUE
  
//  layer.children.add(line)
  layer.children.add(quad)

  private def moveXSource(x: Double) {
    x1 = x
    line.setStartX(x1)
    quad.setStartX(x1)
    quad.setControlX(min(x1,x2) + abs((x2-x1)/2) + 20)
    doMove()
  }
  private def moveYSource(y: Double) {
    y1 = y
    line.setStartY(y1)
    quad.setStartY(y1)
    quad.setControlY(min(y1,y2) + abs((y2-y1)/2) + 20)
    doMove()
  }
  private def moveXTarget(x: Double) {
    x2 = x
    line.setEndX(x2)
    quad.setEndX(x2)
    quad.setControlX(min(x1,x2) + abs((x2-x1)/2) + 20)
    doMove()
  }
  private def moveYTarget(y: Double) {
    y2 = y
    line.setEndY(y2)
    quad.setEndY(y2)
    quad.setControlY(min(y1,y2) + abs((y2-y1)/2) + 20)
    doMove()
  }

  private def doMove() {
  }
  
  def belongs(ag: Ag) = source.ag == ag || target.ag == ag
  def belongs(ag1: Ag, ag2: Ag) = 
    (source.ag == ag1 && target.ag == ag2) || (source.ag == ag2 && target.ag == ag1)
}
