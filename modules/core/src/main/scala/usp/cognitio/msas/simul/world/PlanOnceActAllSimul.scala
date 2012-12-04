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
    var iteration = 0
    world.populate()
    while (!world.satisfied() && iteration < 100) {
      world.act()
      iteration += 1
    }

    val simulNum = 1
    val cy = cycles()

    //    var simulcsv = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-simul.csv", true));
    var simulcsv = new BufferedWriter(new FileWriter("C:\\work\\dev\\pessoal\\msas\\log\\msas-simul.csv", true));
    simulcsv.write(
      simulNum + ","
        + cy._1 + ","
        + cy._2 + ","
        + cy._3
        + "\n")
    simulcsv.close()
  }

  /**
   * Qtd of cycles: (aval, coligate, replan)
   */
  def cycles(): (Int, Int, Int) = {
    var qaval = 0
    var qcolig = 0
    var qreplan = 0
    world.ags.foreach(ag => {
      qaval += ag.qtdAval
      qcolig += ag.qtdColigate
      qreplan += ag.qtdReplan
    })

    (qaval / world.N, qcolig / world.N, qreplan / world.N)
  }

}