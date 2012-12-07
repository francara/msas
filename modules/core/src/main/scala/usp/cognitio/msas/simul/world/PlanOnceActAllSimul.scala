package usp.cognitio.msas.simul.world
import usp.cognitio.msas.env.specific.PlanOnceActAllWorld
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag
import java.io.BufferedWriter
import java.io.FileWriter
import usp.cognitio.msas.simul.compartilhamento.SimulConfiguration

object PlanOnceActAllSimul {

  val world = new PlanOnceActAllWorld(15, 30) {
    override def mean: Double = R / 2
    override def sigma: Double = mean / 2
    override def kmean = 0.1 * mean
    override def ksigma = kmean / 2

    override def randRcAg(ag: Ag): Rc = {
      var rc_ks: List[Int] = Nil
      for (k <- 0 to Rc.DIM - 1) {
        var distrib = rands(k).nextGaussian(mean, sigma).asInstanceOf[Int]
        if (distrib < 0) distrib = distrib * -1
        rc_ks = distrib :: rc_ks
      }

      Rc(rc_ks)
    }
  }

  def main(args: Array[String]) {
    var startWellfare = 0D
    var simulNum = 1

    for (simulNum <- 1 to 10) {
      var iteration = 1
      world.populate()
      while (!world.satisfied() && iteration < 100) {
        world.act()
        if (iteration == 1) {
          world.ags.foreach(_.tracePhy())
          startWellfare = world.wellfare
        }

        iteration += 1
      }
      world.ags.foreach(_.tracePhy())

      val cy = cycles()
      val coalitions = world.coals.map(_._2).toSet.size
      val endWellfare = world.wellfare

      //    var simulcsv = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-simul.csv", true));
      var simulcsv = new BufferedWriter(new FileWriter("C:\\work\\dev\\pessoal\\msas\\log\\msas-simul.csv", true));
      simulcsv.write(
        simulNum + ","
          + cy._1 + ","
          + cy._2 + ","
          + cy._3 + ","
          + coalitions + ","
          + startWellfare + ","
          + endWellfare
          + "\n")
      simulcsv.close()
    }
  }

  /**
   * Qtd of cycles: (aval, coligate, replan)
   */
  def cycles(): (Int, Double, Double) = {
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

}