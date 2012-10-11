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

trait Layer extends Group
/**
 * Show a coalition in a world.
 */
case class CoalLayer(world: AgWorld, coal: Coalition) extends Layer {
  var members = scala.collection.mutable.Map.empty[Ag, Member]
  def ags = members.keys.map(ag => ag)

  var assocs = scala.collection.mutable.Map.empty[Ag, List[Association]]

  val association = Association(this, coal.members(0), coal.members(1))

  coal.members.foreach(ag => {
    val agent = world.agent(ag.id)
    members(ag) = Member(coal, agent)

    agent.centerX.addListener(new ChangeListener[Number] {
      override def changed(obs: javafx.beans.value.ObservableValue[_ <: Number], ovl: Number, nvl: Number) {
        members(agent.ag).centerX = nvl.doubleValue()
        association.moveX(agent.ag, nvl.doubleValue())
      }
    })
    agent.centerY.addListener(new ChangeListener[Number] {
      override def changed(obs: javafx.beans.value.ObservableValue[_ <: Number], ovl: Number, nvl: Number) {
        members(agent.ag).centerY = nvl.doubleValue()
        association.moveY(agent.ag, nvl.doubleValue())
      }
    })

    children.add(members(ag))
  })

}

case class Association(val layer: CoalLayer, val source: Ag, val target: Ag) {
  def moveX(ag: Ag, x: Double) = if (ag == source) moveXSource(x) else moveXTarget(x)
  def moveY(ag: Ag, y: Double) = if (ag == source) moveYSource(y) else moveYTarget(y)

  var (x1, x2, y1, y2) = (0.0, 0.0, 0.0, 0.0)

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
}
