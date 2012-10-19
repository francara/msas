package usp.cognitio.msas.view.ui
import javafx.stage.StageStyle
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import scalafx.stage.Stage
import scalafx.scene.Scene
import usp.cognitio.msas.view.world.Agent
import javafx.stage.Modality

case class AgWindow(ag: Agent) {

  val pane = new BorderPane()
  pane.center = createBody()
  pane.getStyleClass().add("modal-dialog")
  pane.maxWidth = 30

  val stage = new Stage(StageStyle.TRANSPARENT) {
    title = "Agent " + ag.ag.id
    scene = new Scene {
      content = pane
    }
    x = ag.cell.world.x.value + ag.cell.x 
    
    y = ag.cell.world.y.value + ag.cell.y
  }
  stage.scene.getValue().getStylesheets().add("usp/cognitio/msas/view/ui/agent.css")
  stage.maxWidth = 30
//  stage.initModality(Modality.WINDOW_MODAL)
  stage.initOwner(ag.cell.world.stage)
  stage.scene.get().setFill(null)
  stage.show()

  protected def createBody(): VBox = {
    val box = new VBox()
    box.children.add(Lb("Agent: " + ag.ag.id))
    box.children.add(Lb("Rc: " + ag.ag.rc))
    box.children.add(Lb("RcPi: " + ag.ag.rcPi))
    box.children.add(Lb("U: " + ag.ag.u))
    return box
  }

  def close() {
    stage.close()
  }
  
}