package usp.cognitio.msas.view.world
import scalafx.scene.shape.Circle
import usp.cognitio.msas.agent.Ag
import scalafx.scene.shape.Rectangle
import scalafx.animation.RotateTransition
import scalafx.scene.paint.Color
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.coal.Coalition

trait Character extends Circle
case class Agent(ag: MsasAg, var cell: Cell) extends Character {
  doPos()
  radius = cell.width.value / 2 - 2
  fill = doColor(ag.u)

  def pos(ncell: Cell) {
    cell = ncell
    fill = doColor(ag.u)
    doPos()
  }

  private def doColor(u: Double) : Color = {
    if (ag.u == 0) Color.RED
    else if (ag.u > 0.0 && ag.u <= 0.25) Color.web("#DEA11D")
    else if (ag.u > 0.25 && ag.u <= 0.5) Color.web("#EDC44A")
    else if (ag.u > 0.5 && ag.u <= 0.75) Color.web("#DED71D")
    else if (ag.u > 0.75 && ag.u <= 0.9) Color.web("#98D143")
    else Color.web("#38700A")
  }
  
  private def doPos() {
    centerX = cell.x.value + cell.width.value / 2
    centerY = cell.y.value + cell.height.value / 2
  }
}

case class Member(coal: Coalition, agent: Agent) extends Character {
  radius = agent.radius.getValue() * 2
  fill = new Color(Color.BLUEVIOLET.opacity(0.3))
  stroke = Color.BLUEVIOLET
  strokeWidth = 2
}

case class Resource(q: Int) extends Rectangle {
  width = 10
  height = 10
  x = 10; y = 10
  rotate = 45
  fill = Color.CHOCOLATE
  stroke = Color.BROWN
  strokeWidth = 2
}

