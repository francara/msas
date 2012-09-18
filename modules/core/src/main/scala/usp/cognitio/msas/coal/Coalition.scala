package usp.cognitio.msas.coal
import usp.cognitio.math.Roundable
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.Ag
import scala.collection._
import org.apache.log4j.Logger
import usp.cognitio.msas.simul.log.trace
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc
import usp.cognitio.msas.Rc.{ :? => :? }
import usp.cognitio.msas.Rc.CommaCommaQuesttoRc

abstract class Coalition(protected var mbs: List[Ag]) extends Roundable with Logging {

  def this(ag: Ag) = this(List(ag))
  def members: List[Ag] = mbs
  val idx = mutable.HashMap[Long, Ag]()
  mbs.foreach((el) => idx += (el.id -> el))

  var contribution = mutable.Map.empty[Ag, Rc]
  mbs.zip(mbs.map(_.rc)).map(el => contribution += (el._1 -> el._2))

  /**
   * Total planned resources.
   */
  def rcPi: Rc = (Rc.zero() /: mbs.map(_.rcPi))(:?(_) + :?(_))
  /**
   * Total own resources.
   */
  def rc: Rc = (Rc.zero() /: mbs.map(_.rc))(_ + _)
  
  /** v(S) */
  def v: Double

  def u : Double = members.map(_.u).sum / members.size
  def u(als : Map[Ag,Rc]) : Double = 
    members.map(ag => {
      if (als.contains(ag)) ag.u(als(ag))
      else 0.00
    }).sum / members.size
  
  /**
   * Shapley calculation.
   * Implemented in the specifics classes.
   */
  def shapley: List[Double]
  def agShapley: List[(Ag, Double)] = this.members.zip(this.shapley)
    // Sort by shapley value and antiguity
    .sort((e1, e2) =>
      if (e1._2 > e2._2) true
      else if (e1._2 == e2._2 && this.index(e1._1) > this.index(e2._1)) true
      else false)

  /** Divides resources according the shapley value. */
  def rcDiv(shp: List[(Ag, Double)]): Map[Long, (Ag, Int)] = {
    var div = mutable.HashMap[Long, (Ag, Int)]()
    shp.map((el) => (el._1, roundUpInt(el._2 * rc.sum))).foreach((elDiv) => div += (elDiv._1.id -> elDiv))
    return div
  }

  def rcDiv: List[(Ag, Int)] = {
    mbs.zip(shapley).map((el) => (el._1, roundUpInt(el._2 * rc.sum)))
  }
  // TODO Optimize - Coalition division agent search.
  def rcDiv(ag: Ag): Int = rcDiv.filter((el) => el._1 == ag).head._2

  def add(ag: Ag): Coalition = { mbs = ag :: mbs; idx += (ag.id -> ag); contribution += (ag -> ag.rc); this }
  def index(ag: Ag): Int = mbs.indexOf(ag)

  // TODO Optimize - Coalition members remove.
  /**
   * Removes the last agent.
   *
   * This method is used to remove an agent that is being tested
   * in the coalition.
   */
  def remove: Coalition = { 
    idx -= mbs.head.id; val ag = mbs.head; mbs = mbs.tail; 
    contribution.remove(ag); 
    this 
  }
  def remove(ag: Ag): Coalition = { 
    idx -= ag.id; mbs = mbs.remove(_ == ag);
    contribution.remove(ag)
    this 
  }

  def alocate(): Map[Ag, Rc] = {
    trace alocate (this)
    if (mbs.size == 1) return contribution

    var als: Map[Ag, Rc] = Map.empty[Ag, Rc]

    // List of resources from the coalition 
    // which are iteratively consumed by agents.
    var toBeConsumed = this.rc
    var agShapleyVls = agShapley
    trace shapley (this, agShapley)

    var divMap = rcDiv(agShapleyVls)
    trace division (this, divMap.values.toList)
    agShapleyVls.foreach(el => {
      // TODO Divisão de recursos não aproveita sobras
      // Para cada iteração considerar uma divisão de 100% dos recursos entre os membros resultantes.
      var q: Int = divMap(el._1.id)._2
      var aloc = el._1.consume(toBeConsumed, q)
      toBeConsumed = toBeConsumed - aloc
      als = als + (el._1 -> aloc)
    })

    trace alocation (this, als.toList)
    trace available (this, toBeConsumed)
    als = alocateAvailable(als, toBeConsumed, agShapleyVls)

    trace alocationAndAvailable (this, als.toList)
    return als
  }

  def alocateAvailable(als: Map[Ag, Rc], avail: Rc, shp: List[(Ag, Double)]): Map[Ag, Rc] = {
    val totavail = avail.sum

    var rest: List[(Ag, Rc)] = Nil
    var rest_round = new Array[Double](Rc.DIM)
    shp.foreach(el => {
      val ag = el._1; val sh = el._2
      val rest_i: List[Int] =
          ag.rc.toList.zipWithIndex.map(el => {
            val rc_k = el._1; val k = el._2
            val rest_i_k = sh * avail(k)
            val rest_round_i_k = rest_i_k - rest_i_k.asInstanceOf[Int]

            rest_round(k) = rest_round(k) + rest_round_i_k

            if (rest_round(k) >= 1) {
              rest_round(k) = rest_round(k) - 1
              rest_i_k.asInstanceOf[Int] + 1
            } else 
              rest_i_k.asInstanceOf[Int]
          })
      rest = (ag, new Rc(rest_i)) :: rest
    })

    trace alocationRest (this, rest)

    // The last rest...
    var availDivided = avail - (Rc.zero() /: rest.map(el => el._2)) (_ + _)
    trace availableAfterRest (this, availDivided)
    var unity = availDivided.toList.forall(rc => rc == 0 || rc == 1)
      
    var alocAndRest = scala.collection.mutable.HashMap.empty[Ag, Rc]
    alocAndRest ++= als.toList
    rest.foreach(el => {
      val ag = el._1; val rc_rest = el._2
      val rc_avail = if (unity && ag == shp(0)._1) availDivided else Rc.zero()
      val rc_al = alocAndRest(ag)
      alocAndRest(ag) = rc_al + rc_rest + rc_avail
    })
    alocAndRest
  }

  def alocate(who: Ag): Rc = alocate()(who)

  def alocateWithWellfare(who: Ag): Map[Ag, Rc] = {
    val isMember = if (members.contains(who)) true else false
    val beforeWell = members.map(_.u).sum / members.size

    var al = alocate()
    
    if (!isMember) add(who)
    al = alocate()
    
    var afterWell = 0.0
    members.filter(_ != who).foreach(ag => {
      afterWell += ag.u(al(ag))
    })
    afterWell = afterWell / (members.size - 1)
    
    if (!isMember) remove(who)
    
    if (afterWell <= beforeWell) {
      al = alocate()
      al += (who -> who.rc)
      return al
    }
    else return al
  }
    
  def distribute(): Map[Ag, Rc] = {
    var als = alocate()
    als.foreach((el) => el._1.rc = el._2)
    return als
  }

  def restore() {
    mbs.foreach(ag => {
        var aloc = ag.rc
        ag.rc = contribution(ag)
      })
  }

  override def toString = mbs.mkString("[", ",", "]")

}

