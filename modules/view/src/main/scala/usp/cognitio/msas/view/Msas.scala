package usp.cognitio.msas.view
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.Scene
import javafx.stage.StageStyle
import usp.cognitio.msas.view.ui.MainWindow
import usp.cognitio.msas.view.ui.SimulWindow

object Msas extends JFXApp {
  
  stage = new Stage {
    title = "MsAs - Simulator"
    scene = new Scene {
      content = new MainWindow() {
        def onStart(once: Boolean, consumable: Boolean, sameTarget: Boolean,
          mean: Double, sigma: Double,
          cmean: Double, csigma: Double) {
        	SimulWindow( 20 + width.doubleValue(), 10)
        }
        def onPlan() {

        }
        def onCoal() {

        }
      }
    }
    x = 10; y = 10
    fullScreen = false
    resizable = false
  }


}