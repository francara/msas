package usp.cognitio.msas.view.world
import usp.cognitio.msas.Rc
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.Group

case class Cell(val world: World, var rc: Rc) extends Rectangle {
  stroke = Color.BROWN
  update()
  
  def update() {
    fill =
      if (!world.config.color) Color.WHITE
      else if (rc.sum == 0) Color.web("#FFFFFF")
      else if (rc.sum == 1) Color.web("#CBCDD1")
      else if (rc.sum == 2) Color.web("#AAADB3")
      else if (rc.sum == 3) Color.web("#8D9196")
      else if (rc.sum == 4) Color.web("#777A80")
      else if (rc.sum == 5) Color.web("#616469")
      else Color.web("4A4D52")
  }
}

case class TargetCell(override val world: World, var _rc: Rc) extends Cell(world,_rc) {
  fill = Color.DARKGREEN
  override def update() {}
}

case class NodeStar(val cell: Cell, var open: Boolean) extends Group {
  val CELL_SCALE = 0.4
  val r1 = new Rectangle {
    x = cell.x.value + cell.width.value/3
    y = cell.y.value + cell.height.value/3
    width = cell.width.value * CELL_SCALE
    height = cell.height.value * CELL_SCALE
    fill = Color.DARKBLUE
    stroke = Color.DARKBLUE
    strokeWidth = 2
  }
  val r2 = new Rectangle {
    x = cell.x.value + cell.width.value/3
    y = cell.y.value + cell.height.value/3
    width = cell.width.value * CELL_SCALE
    height = cell.height.value * CELL_SCALE
    fill = if (open) Color.WHITE else (Color.DARKBLUE)
    stroke = Color.DARKBLUE
    strokeWidth = 2
    rotate = 45    
  }
  
  children.add(r1)
  children.add(r2)
}