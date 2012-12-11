package usp.cognitio.msas.env.specific
import usp.cognitio.msas.Rc
import java.io.BufferedWriter
import usp.cognitio.msas.env.specific.PlanAg
import java.io.FileWriter

class Plan2OnceActAllWorld(val _N: Int, private val __r: Int) extends PlanActWorld(_N, __r) {
  override def create(id: Int): PlanAg = new PlanAg(this, id, Rc()) with PlanOnceActAllBehaviour
}

class PlanCompleteActAllWorld(val _N: Int, private val __r: Int) extends PlanActWorld(_N, __r) {
  override def create(id: Int): PlanAg = new PlanAg(this, id, Rc()) with PlanCompleteActAllBehaviour
}

class PlanCompleteActReplanWorld(val _N: Int, private val __r: Int) extends PlanActWorld(_N, __r) {
  override def create(id: Int): PlanAg = new PlanAg(this, id, Rc()) with PlanCompleteActReplanBehaviour
}

object Worlds {
  val N = 15
  val R = 30
  var meanTarget = 10
  var kmeanScale = 0.05
  var kmeans = List(0.05, 0.075, 0.1, 0.125, 0.15)
  val worlds = 
    new Plan2OnceActAllWorld(N, R) ::
    new PlanCompleteActAllWorld(N, R) ::
    new PlanCompleteActReplanWorld(N, R) ::
    Nil

  def main(args: Array[String]) {
    var startWellfare = 0D
    var startLack = 0D
    var worldIndex = 0
    var simulNum = 0

    for (worldIndex <- 0 to worlds.size-1) {
      var world = worlds(worldIndex)

      for (meanIndex <- 0 to 4) {
    	world.kmeanScale = kmeans(meanIndex)
        
        for (simulNum <- 1 to 10) {
          var iteration = 1
          world.populate()
          while (!world.done() && iteration < 100) {
            world.act()
            if (iteration == 1) {
              world.ags.foreach(_.tracePhy())
              startWellfare = world.wellfare
              startLack = world.lack
            }

            iteration += 1
          }

          sumarize(world, worldIndex, simulNum)
          tracerc(world)
        }
      }
    }

    def sumarize(world: PlanActWorld, worldIndex: Int, simulNum: Int) {
      /**
       * Qtd of cycles: (aval, coligate, replan)
       */
      def cycles(world: PlanActWorld): (Int, Double, Double) = {
        var qaval = 0
        var qcolig = 0
        var qreplan = 0
        world.ags.foreach(ag => {
          qaval += ag.qtdAval
          qcolig += ag.qtdColigate
          qreplan += ag.qtdReplan
        })

        (qaval / world.N, qcolig / world.N.asInstanceOf[Double], qreplan / world.N.asInstanceOf[Double])
      }

      world.ags.foreach(_.tracePhy())

      val cy = cycles(world)
      val coalitions = world.coals.map(_._2).filter(_.members.size > 1).toSet.size
      val endWellfare = world.wellfare
      val endLack = world.lack

      //    var simulcsv = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-simul.csv", true));
      var simulcsv = new BufferedWriter(new FileWriter("C:\\work\\dev\\pessoal\\msas\\log\\msas-simul.csv", true));
      simulcsv.write(
        worldIndex + ";"
          + simulNum + ";"
          + world.mean + ";"
          + world.kmean + ";"
          + cy._1 + ";"
          + cy._2 + ";"
          + cy._3 + ";"
          + coalitions + ";"
          + startLack + ";"
          + endLack + ";"
          + startWellfare + ";"
          + endWellfare
          + "\n")
      simulcsv.close()
    }

    def tracerc(world: PlanActWorld) {

      var simulcsv = new BufferedWriter(new FileWriter("C:\\work\\dev\\pessoal\\msas\\log\\msas-rc.csv", true));
      world.ags.foreach(ag => {
        simulcsv.write(
          ag.id + ";"
            + ag.rc(0) + ";"
            + ag.rc(1) + ";"
            + ag.rc(2) + ";"
            + ag.rcPi(0) + ";"
            + ag.rcPi(1) + ";"
            + ag.rcPi(2)
            + "\n")

      })
      simulcsv.close()
    }

  }

}