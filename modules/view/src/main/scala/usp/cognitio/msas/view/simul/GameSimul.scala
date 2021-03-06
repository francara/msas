package usp.cognitio.msas.view.simul
import scala.util.Random
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.stage.Stage
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.specific.PlanOnceActAllWorld
import usp.cognitio.msas.Rc
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.view.world._


object GameView extends JFXApp {

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
          else  rands(k).nextGaussian(mean, sigma).asInstanceOf[Int]
        if (distrib < 0) distrib = distrib * -1
        rc_ks = distrib :: rc_ks
      }

      Rc(rc_ks)
    }

  }
  grid.populate()

  val (ag1, ag2) = (grid.ag(1), grid.ag(2))
  val coalition = new Coalition(List(ag1, ag2)) {
      override def shapley: List[Double] = 0.0 :: 0.0 :: Nil
      override def v: Double = 0.0
  }
  
  val world = AgWorld(grid, 10, 10)
  stage = new Stage {
    title = "Simula��o"
    scene = new Scene {
      content = world :: world.agents
    }
  }
  
  val layer = CoalLayer(world, coalition)
  world.addLayer(layer)

  new Thread() {
    override def run() {
      while (!grid.satisfied()) {
        grid.act()
        world.update()
        Thread.sleep(1500)
      }
    }
  }.start()

}