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
import scalafx.stage.Stage
import javafx.stage.StageStyle
import scalafx.scene.Scene
import usp.cognitio.msas.view.ui.AgWindow

/*
 *********************************** 
 ************   World   ************ 
 *********************************** 
 */
case class WConfig(val grid: Boolean = true, val color: Boolean = true)
case class World(val vworld: GridWorld, _x: Double, _y: Double) extends Group {
  def N = vworld.ags.size
  def R = vworld.R
  var config = WConfig(true, true)
  val cellWidth = if (25 * vworld.R > 1024) 1024 / (2 * vworld.R) else 25
  val cellHeight = if (25 * vworld.R > 1048) 1024 / (2 * vworld.R) else 25
  val cellStroke = 0.5

  var cells: Array[Array[Cell]] = Array.tabulate(vworld.R, vworld.R)((x, y) => null)

  def cell(x: Int, y: Int) = cells(x)(y)
  def cell(p: Point) = cells(p.x)(p.y)

  val width = (cellWidth + 2 * cellStroke) * vworld.R
  val height = (cellHeight + 2 * cellStroke) * vworld.R
  children = new Rectangle() {
    width = (cellWidth + 2 * cellStroke) * vworld.R
    height = (cellHeight + 2 * cellStroke) * vworld.R
    fill = Color.LIGHTGRAY;
  } :: Nil

  /*
   * Cells.
   */
  vworld.cells.foreach(_.foreach(cell => {
    val cx = (cellWidth + 2 * cellStroke) * cell.x
    val cy = (cellHeight + 2 * cellStroke) * cell.y

    cells(cell.x)(cell.y) = new Cell(this, cell.rc) {
      def width = cellWidth
      def height = cellHeight
      def x = cx
      def y = cy
      def strokeWidth = cellStroke
    }
    children.add(cells(cell.x)(cell.y))
  }))

  def world = this
  val stage = new Stage(StageStyle.UTILITY) {
    title = "Simulação"
    scene = new Scene {
      content = world
    }
    x = _x; y = _y
  }
  stage.show()

  def x = stage.x
  def y = stage.y

  def update() = cells.foreach(_.foreach(_.update()))
  
  var layers: List[Layer] = Nil
  
  def containsLayer(layer: Layer) = layers.contains(layer)
  def addLayer(layer: Layer) = {
    layers = layer :: layers
    children.add(layer)
  }
  def removeLayer(layer: Layer) = {
    layers = layers.remove(_ == layer)
    children.remove(layer)
  }
}

/*
 ************************************* 
 ************   AgWorld   ************ 
 ************************************* 
 */
case class AgWorld(override val vworld: GridWorld, override val _x: Double, override val _y: Double)
  extends World(vworld, _x, _y) {
  var ags = scala.collection.mutable.Map.empty[Ag, Agent]

  var agWindow: AgWindow = null

  def agents: List[Character] = ags.values.toList
  def agent(id: Long): Agent = ags.find(el => el._1.id == id).get._2

  /*
   * Agents.
   */
  vworld.ags.foreach(ag => {
    val vcell = vworld.whereIs(ag)
    val cell = cells(vcell.x)(vcell.y)
    val agent = Agent(ag, cell)
    ags(ag) = agent
    world.children.add(agent)
  })
  
  addLayer(PhaseLayer(this))

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

  def showWindow(agent: Agent) {
    if (agWindow != null) agWindow.close()
    agWindow = new AgWindow(agent)
  }
}

/*
 *************************************** 
 ************   PlanWorld   ************ 
 *************************************** 
 */
case class PlanWorld(val _v: GridWorld, override val _x: Double, override val _y: Double)
  extends World(_v, _x, _y) {
  var nodes: Array[Array[NodeStar]] = Array.tabulate(vworld.R, vworld.R)((x, y) => null)

  def open(pos: Point): NodeStar = createNode(pos, true)
  def close(pos: Point): NodeStar = createNode(pos, false)
  private def createNode(pos: Point, open: Boolean): NodeStar = {
    val cell = cells(pos.x)(pos.y)
    val node = NodeStar(cell, open)
    nodes(pos.x)(pos.y) = node
    children.add(node)
    node
  }

}
