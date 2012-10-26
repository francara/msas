package usp.cognitio.msas.view
import scalafx.application.JFXApp
import scalafx.stage.Stage
import scalafx.scene.Scene
import javafx.stage.StageStyle
import usp.cognitio.msas.view.ui.MainWindow
import usp.cognitio.msas.view.ui.SimulWindow
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.view.ui.SimulWindow
import usp.cognitio.msas.view.ui.CoalWindow
import usp.cognitio.msas.env.specific.PlanOnceActAllWorld
import org.apache.log4j.Logger
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.view.world.CoalLayer

object Msas extends JFXApp {
  val logger = Logger.getLogger(getClass().getName());

  var world: PlanOnceActAllWorld = null

  var simul: SimulWindow = null
  var coal: CoalWindow = null

  stage = new Stage {
    title = "MsAs - Simulator"
    scene = new Scene {
      content = new MainWindow() {
        def onStart(once: Boolean, consumable: Boolean, sameTarget: Boolean,
          mean: Double, sigma: Double,
          cmean: Double, csigma: Double,
          phySema: Boolean, socSema: Boolean) {
          simul = SimulWindow(20 + width.doubleValue(), 10, phySema, socSema)
          world = simul.grid
        }
        def onPlan() {

        }
        def onCoal() {
          coal = CoalWindow(world)
        }
        def onSema(phySema: Boolean, socSema: Boolean) {
          if (simul == null) return
          simul.grid.phySem = phySema
          simul.grid.socialSem = socSema
        }

      }
    }
    x = 10; y = 10
    fullScreen = false
    resizable = false
  }

  var coalLayer: CoalLayer = null
  def showCoal(coal: Coalition) {
    logger.debug("Show coal: " + coal)
    
    if (coalLayer != null) {
      simul.world.removeLayer(coalLayer)
    }
    val layer = CoalLayer(simul.world, coal)
    simul.world.addLayer(layer)
    coalLayer = layer
  }

  def removeCoal {
    if (coalLayer != null) {
      simul.world.removeLayer(coalLayer)
    }
  }

  def hideCoal {
    coal.hide()
  }
  
  def showAg(ag: MsasAg) {

  }
}