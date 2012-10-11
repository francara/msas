package usp.cognitio.msas.view.world
import usp.cognitio.msas.env.GridWorld
import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import sun.security.krb5.Config
import usp.cognitio.math.alg.Point
import scalafx.scene.Group

/*
 *********************************** 
 ************   World   ************ 
 *********************************** 
 */
case class WConfig(val grid: Boolean = true, val color: Boolean = true)
case class World(val vworld: GridWorld) extends Group {
  def N = vworld.ags.size
  def R = vworld.R
  var config = WConfig(true, true)
  val cellWidth = if (25 * vworld.R > 1024) 1024 / (2 * vworld.R) else 25
  val cellHeight = if (25 * vworld.R > 1048) 1024 / (2 * vworld.R) else 25
  val cellStroke = 0.5

  var cells: Array[Array[Cell]] = Array.tabulate(vworld.R, vworld.R)((x, y) => null)

  def cell(x:Int, y:Int) = cells(x)(y)
  def cell(p: Point) = cells(p.x)(p.y)
  
  val width = (cellWidth + 2 * cellStroke) * vworld.R
  val height = (cellHeight + 2 * cellStroke) * vworld.R
  children = new Rectangle() {
    width = (cellWidth + 2 * cellStroke) * vworld.R
    height = (cellHeight + 2 * cellStroke) * vworld.R
    fill = Color.LIGHTGRAY;
  } :: Nil

  def content: List[Rectangle] = cells.toList.flatMap(_.toList)

  /*
   * Cells.
   */
  vworld.cells.foreach(_.foreach(cell => {
    val cx = (cellWidth + 2 * cellStroke) * cell.x
    val cy = (cellHeight + 2 * cellStroke) * cell.y

    cells(cell.x)(cell.y) = new Cell(this, cell.rc) {
      width = cellWidth
      height = cellHeight
      x = cx; y = cy
      strokeWidth = cellStroke
    }

    children.add(cells(cell.x)(cell.y))
    
  }))

  def update() = cells.foreach(_.foreach(_.update()))
  def addLayer(layer: Layer) = children.add(layer)
}

/*
 ************************************* 
 ************   AgWorld   ************ 
 ************************************* 
 */
case class AgWorld(override val vworld: GridWorld) extends World(vworld) {
  var ags = scala.collection.mutable.Map.empty[Ag, Agent]

  def agents: List[Character] = ags.values.toList
  def agent(id: Long): Agent = ags.find(el => el._1.id == id).get._2

  /*
   * Agents.
   */
  vworld.ags.foreach(ag => {
    val vcell = vworld.whereIs(ag)
    val cell = cells(vcell.x)(vcell.y)
    ags(ag) = Agent(ag, cell)
  })

  def whereIs(ag: Ag): Cell = {
    val vcell = vworld.whereIs(ag)
    val cell = cells(vcell.x)(vcell.y)
    cell
  }

  override def update() {
    super.update()
    vworld.ags.foreach(ag => {
      agent(ag.id).pos(whereIs(ag))
    })

  }
}

/*
 *************************************** 
 ************   PlanWorld   ************ 
 *************************************** 
 */
case class PlanWorld(val _v: GridWorld) extends World(_v) {
  var nodes: Array[Array[NodeStar]] = Array.tabulate(vworld.R, vworld.R)((x, y) => null)

  def open(pos: Point): NodeStar = createNode(pos, true)
  def close(pos: Point): NodeStar = createNode(pos, false)
  private def createNode(pos: Point, open:Boolean): NodeStar = {
    val cell = cells(pos.x)(pos.y)
    val node = NodeStar(cell, open)
    nodes(pos.x)(pos.y) = node
    children.add(node)
    node
  }

}
