package usp.cognitio.msas.coal
import scala.Math.min
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.util.Permute
import usp.cognitio.math.Roundable
import usp.cognitio.msas.Rc

class VoteCoalition(_ags: List[Ag]) extends Coalition(_ags) with Roundable {
  def this(ag: Ag) = this(List(ag))

  def _quota: Int = min(rcPi.sum, this.rc.sum)
  def _weight: List[Int] = mbs.map(ag => ag.rc.sum)
  def quota(dim: Int) = min(rcPi(dim), rc(dim))

  // TODO Implement VoteCoalition v.
  def v: Double = {
    0.00
  }

  override def shapley: List[Double] = {
    // TODO Optimize - Permutations
    val perms: List[List[Ag]] = permutations
    val shValue: Array[Double] = new Array[Double](mbs.length)
    mbs.foreach(ag => {
      var pivotCount: Double = 0.0
      perms.foreach(perm => {
        // Ordering: sublist without ag
        val ord: List[Ag] = perm.takeWhile(_ != ag)

        for (dim <- 0 to Rc.DIM - 1) {
          // Sum ordering's resources
          val ordWeight: Int = Rc.sum(ord.map(_.rc), dim)
          /*
           * Ordering PIVOT.
           * Rule:
           */
          if (ord.length == 0 && quota(dim) == 0) pivotCount += 1
          else if (ordWeight < quota(dim) && (ordWeight + ag.rc(dim)) >= quota(dim)) pivotCount += 1
        }
      })
      shValue(mbs.indexOf(ag)) = round(pivotCount / (perms.length * Rc.DIM))
    })
    return norm(shValue.toList)
  }

  private def norm(shps: List[Double]): List[Double] = {
    var sum = (0.0 /: shps)(_ + _)
    if (sum == 0) return shps
    else return shps.map((sh) => round(sh / sum))
  }

  def _shapley: List[Double] = {
    // TODO Optimize - Permutations
    val perms: List[List[Ag]] = permutations
    val shValue: Array[Double] = new Array[Double](mbs.length)
    mbs.foreach(ag => {
      var pivotCount: Double = 0.0
      perms.foreach(perm => {
        // Ordering: sublist without ag
        val ord: List[Ag] = perm.takeWhile(_ != ag)

        // Sum ordering's resources
        val ordWeight: Int = Rc.sum(ord.map(_.rc))

        /*
         * Ordering PIVOT.
         * Rule:    
         */
        if (ordWeight < _quota && (ordWeight + ag.rc.sum) >= _quota) pivotCount += 1
      })
      shValue(mbs.indexOf(ag)) = round(pivotCount / perms.length)
    })
    return shValue.toList
  }

  /**
   * Build all possible orderings of agents.
   */
  private def permutations: List[List[Ag]] = new Permute(mbs).go

}