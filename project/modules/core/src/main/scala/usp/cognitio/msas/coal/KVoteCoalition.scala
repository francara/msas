package usp.cognitio.msas.coal
import scala.Math.min
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.util.Permute
import usp.cognitio.math.Roundable
import usp.cognitio.msas.Rc

class KVoteCoalition(_ags: List[Ag]) extends Coalition(_ags) with Roundable {
  def this(ag: Ag) = this(List(ag))

  def _quota: Int = min(rcPi.sum, this.rc.sum)
  def _weight: List[Int] = mbs.map(ag => ag.rc.sum)
  def quota(dim: Int) = min(rcPi(dim), rc(dim))

  // TODO Implement VoteCoalition v.
  def v: Double = {
    0.00
  }

  override def shapley: List[Double] = {
    val perms: List[List[Ag]] = permutations
    val shValue: Array[Double] = new Array[Double](mbs.length)
    mbs.foreach(ag => {
      // Count of pivots in all current member orderings.
      var pivotCount: Double = 0.0
      perms.foreach(perm => {
        // Ordering: sublist without ag
        val ord: List[Ag] = perm.takeWhile(_ != ag)

        var winning = Array.fill[Boolean](Rc.DIM)(false)
        val ordWeight = Array.fill[Int](Rc.DIM)(0)
        for (dim <- 0 to Rc.DIM - 1) {
          // Sum ordering's resources
          ordWeight(dim) = Rc.sum(ord.map(_.rc), dim)
          /*
           * Ordering PIVOT.
           * Rule:
           */
          val q = quota(dim)
          if (ord.length == 0 && quota(dim) == 0) winning(dim) = true
          else if (ordWeight(dim) >= quota(dim)) winning(dim) = true
        }

        val loosing:Array[(Boolean,Int)] = winning.zipWithIndex.filter(el => el._1 == false)
        // There are some dimensions which are not winning.
        if (loosing.length > 0) {
          val pivot:Boolean = loosing.forall(el => {
            ordWeight(el._2) + ag.rc(el._2) >= quota(el._2) 
          })
          if (pivot) pivotCount += 1
        }

      }) // foreach order
      shValue(mbs.indexOf(ag)) = round(pivotCount / perms.length)
    }) // foreach member
    return norm(shValue.toList)
  }

  private def norm(shps: List[Double]): List[Double] = {
    var sum = (0.0 /: shps)(_ + _)
    if (sum == 0) return shps
    else return shps.map((sh) => round(sh / sum))
  }

  /**
   * Build all possible orderings of agents.
   */
  private def permutations: List[List[Ag]] = new Permute(mbs).go

}