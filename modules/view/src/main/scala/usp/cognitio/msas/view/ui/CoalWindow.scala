package usp.cognitio.msas.view.ui
import usp.cognitio.msas.view.ui.CoalWindow._
import scalafx.scene.layout.VBox
import scalafx.scene.layout.BorderPane
import scalafx.stage.Stage
import javafx.stage.StageStyle
import scalafx.scene.Scene
import usp.cognitio.msas.env.GridWorld
import scalafx.scene.control.ComboBox
import scalafx.scene.layout.GridPane
import javafx.geometry.Insets
import javafx.event.EventHandler
import javafx.event.ActionEvent
import scalafx.collections.ObservableBuffer
import javafx.scene.input.MouseEvent
import usp.cognitio.msas.view.Msas
import scalafx.scene.control.Button

case class CoalWindow(world: GridWorld) {
  val pane = new BorderPane()
  pane.center = createBody()
  pane.prefWidth = WIDTH

  val stage = new Stage(StageStyle.DECORATED) {
    title = "Coalitions"
    scene = new Scene {
      content = pane
    }
    height = HEIGHT
    width = WIDTH
    //    x = ag.cell.world.x.value + ag.cell.x 
    //    y = ag.cell.world.y.value + ag.cell.y
  }
  stage.show()

  protected def createBody(): VBox = {
    /*
     * Coalitions
     */
    val coalitions = new ComboBox(world.coals.values.toList) {
      onAction = new EventHandler[ActionEvent] {
        def handle(event: ActionEvent) {
          items = ObservableBuffer(world.coals.values.toList)
        }
      }
    }
    val bt = new Button("Show") {
      onAction = new EventHandler[ActionEvent] {
        def handle(event: ActionEvent) {
          if (coalitions.getValue() == null) return
          Msas.showCoal(coalitions.getValue())
        }
      }
    }
    bt.setPrefSize(80, 20);
    
    /*
     * Agents
     */
    val agents = new ComboBox(world.ags) 
    val btAg = new Button("Show") {
      onAction = new EventHandler[ActionEvent] {
        def handle(event: ActionEvent) {
          if (agents.getValue() == null) return
          Msas.showAg(agents.getValue())
        }
      }
    }
    btAg.setPrefSize(80, 20);

    val box = new VBox()

    val grid = new GridPane {
      hgap = 10; vgap = 10
      padding = new Insets(10, 10, 10, 10)

      add(Lb("Coalitions:"), 1, 1)
      add(coalitions, 2, 1)
      add(bt, 3, 1)
      
      add(Lb("Agents:"), 1, 2)
      add(agents, 2, 2)
      add(btAg, 3, 2)
      
    }
        
    box.children.add(grid)
    return box
  }

  def close() = stage.close()
  def hide() = stage.hide

}

object CoalWindow {
  val WIDTH = 320
  val HEIGHT = 150
}
