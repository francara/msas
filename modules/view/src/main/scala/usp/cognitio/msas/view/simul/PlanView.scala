package msas.view
import scalafx.application.JFXApp
import usp.cognitio.msas.env.specific.PlanOnceActAllWorld
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import scala.util.Random
import scalafx.stage.Stage
import scalafx.scene.Scene
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.cog.plan.AStarListeners
import scalafx.concurrent.Task
import javafx.application.Platform
import scalafx.scene.paint.Color
import usp.cognitio.msas.view.world._


object PlanView extends JFXApp {

  Rc.DIM = 5
  val grid = new PlanOnceActAllWorld(15, 30) {
    override def mean: Double = R / 2
    override def sigma: Double = mean / 2
    override def kmean = 0.1 * mean
    override def ksigma = kmean / 2
    override def randRcAg(ag: Ag): Rc = {
      val rand = new Random()
      var rc_ks: List[Int] = Nil
      for (k <- 0 to Rc.DIM - 1) {
        var distrib =
          if (rand.nextInt(5) <= 2) 0
          else rands(k).nextGaussian(mean, sigma).asInstanceOf[Int]
        if (distrib < 0) distrib = distrib * -1
        rc_ks = distrib :: rc_ks
      }

      Rc(rc_ks)
    }
    def punish(p: Boolean) = ags.foreach(_.ecog.enablePunishment = p)

  }
  grid.populate()

  val (ag1, ag2) = (grid.ag(1), grid.ag(2))

  val world = new PlanWorld(grid, 10, 10) {
    config = WConfig(true, false)
//    cells(ag1.target.x)(ag1.target.y) = TargetCell(this, Rc.nil)
    update()
  }

  stage = new Stage {
    title = "Simulação"
    scene = new Scene {
      content = world
    }
  }
  stage.show()

  AStarListeners.open = { (current, target) =>
    Thread.sleep(10)
    Platform.runLater(new Runnable(){
      override def run() {
    	  world.open(current)
      }
    })
  }
  AStarListeners.close = { (current, target) =>
    Platform.runLater(new Runnable(){
      override def run() {
    	  world.close(current)
      }
    })
  }

  grid.punish(false)
  val task = Task {
    val sense = grid.sense(ag1)
    ag1.ecog.build(sense)
  }
  
  new Thread(task).start();

}