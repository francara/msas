package usp.cognitio.msas.simul.compartilhamento
import usp.cognitio.msas.env.specific.PlanOnceActAllWorld
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.Ag

class PlanOnceActAllSimul {

  val world = new PlanOnceActAllWorld(20, 50) {
    override def mean: Double = R / 2
    override def sigma: Double = mean / 2

    override def randRcCell(x: Int, y: Int): Rc = {
      if (randCell == null) randCell = new RandomDataImpl()
      val k = randCell.nextInt(0, Rc.DIM - 1)
      Rc((0 until Rc.DIM).toList.map(v => if (v == k) 1 else 0))
    }
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
    world.populate()
    
    
    
    world.ags.foreach(ag => {
      ag.esoc.act(null, null)
    })
  }
}