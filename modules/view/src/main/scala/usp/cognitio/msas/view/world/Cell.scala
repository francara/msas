package usp.cognitio.msas.view.world
import usp.cognitio.msas.Rc
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.Group
import scalafx.scene.control.Label

abstract case class Cell(val world: World, var rc: Rc) extends Group {
  def width: Double
  def height : Double
  def x : Double
  def y : Double
  def cell = this
  
  val rec = new Rectangle {
	 stroke = Color.BROWN
	 x = cell.x
	 y = cell.y
	 width = cell.width
	 height = cell.height
  }
  children.add(rec)

  update()
  
  def update() {
    rec.fill =
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

case class Stucked(cell: Cell) extends Group {
  val CELL_SCALE = 0.4
  val rec = new Rectangle {
    width = cell.width * CELL_SCALE
    height = cell.height * CELL_SCALE
    x = cell.x
    y = cell.y + cell.height - cell.height * CELL_SCALE
    fill = Color.rgb(108,127,184, 0.4)
    stroke = Color.rgb(108,127,184)
    strokeWidth = 0.5
  }
  val lb = new Label {
    text = "Sk"
    
  }
  children.add(rec)
  children.add(lb)  
}


case class NodeStar(val cell: Cell, var open: Boolean) extends Group {
  val CELL_SCALE = 0.4
  val r1 = new Rectangle {
    x = cell.x + cell.width/3
    y = cell.y + cell.height/3
    width = cell.width * CELL_SCALE
    height = cell.height * CELL_SCALE
    fill = Color.DARKBLUE
    stroke = Color.DARKBLUE
    strokeWidth = 2
  }
  val r2 = new Rectangle {
    x = cell.x + cell.width/3
    y = cell.y + cell.height/3
    width = cell.width * CELL_SCALE
    height = cell.height * CELL_SCALE
    fill = if (open) Color.WHITE else (Color.DARKBLUE)
    stroke = Color.DARKBLUE
    strokeWidth = 2
    rotate = 45    
  }
  
  children.add(r1)
  children.add(r2)
}