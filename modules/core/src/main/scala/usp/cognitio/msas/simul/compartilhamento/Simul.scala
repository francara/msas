package usp.cognitio.msas.simul.compartilhamento
import scala.collection.mutable
import scala.util.Random
import usp.cognitio.math.Roundable
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.soc.Aval
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.coal.KLinearSampleCoalitionGame
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.simul.log.trace
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc

/**
 * Sharing simulation.
 * --------------------
 * This simulation rondomily creates agents with resources.
 * The agents interact with his neighborhood and select a candidate partner.
 * Coaltions are formed if there is an increment in the agent's utility.
 *
 * Verifications:
 *    1. Increase in social-wellfare.
 *
 * Configurations:
 *    1.
 *    2. Lack of resources
 */
object Simul extends Logging with Roundable {

  Socialization((ags: List[Ag]) => new KLinearSampleCoalitionGame(ags))
  //  Socialization((ags: List[Ag]) => new KSampleVoteCoalition(ags))
  //  Socialization((ags: List[Ag]) => new KVoteCoalition(ags))
  //  Socialization((ags: List[Ag]) => new VoteCoalition(ags))
  //  Socialization((ags: List[Ag]) => new SampleVoteCoalition(ags, MEAN, VARIANCE))

  var agents: List[Socialization] = Nil

  def main(args: Array[String]) {
    //    config = SimulConfiguration()
    SimulConfiguration.read("src/main/resources/msas-config-fast.simul")
    while (SimulConfiguration.hasNext()) {
      SimulConfiguration.next

      trace.rollover()

      init()
      run(SimulConfiguration.ITERATIONS, SimulConfiguration.ITERATIONS_3)
    }
  }

  def init() {
    Rc.DIM = SimulConfiguration.DIM

    agents = createAgentsNormalDistr()
  }

  def run(numIterations: Int*) {
    var iterations = 1; var iterations3 = 0
    var iteration = 0
    trace start (agents)
    if (numIterations.length != 0) iterations = numIterations(0)
    if (numIterations.length > 1) iterations3 = numIterations(1)

    var currentWell = 0.00
    var countUnchangedIterations = 0
    for (currentIteration <- 0 to iterations-1) {
      iteration += 1
      if (currentWell == wellfare) countUnchangedIterations += 1

      currentWell = iterate(iteration)
      if (countUnchangedIterations == 2) {
        reposition()
        countUnchangedIterations = 0
      }
    } // Iteration
    
    trace finish
  }

  private def iterate(iteration: Int): Double = {
    info("Iteration " + iteration); trace iteration (iteration)
    agents.foreach((ag) => {
      trace init (ag)
      // Coalitions already tested...
      var testedCoals = Map.empty[Coalition, Aval]

      // obs: filter yields all the elements with predicate true
      // takes elements different from himself and wich aren't part of his own coaltion.        
      val neighs =
        agents.filter(_ != ag).filter((neigh) => !ag.coalition.members.contains(neigh))

      var topNeigh: Socialization = null
      var topUAg: Double = -1.0
      neighs.foreach((neigh) => {
        //val (uNeigh, uAg, uCoal) = neigh.avaliate(ag)
        if (!testedCoals.contains(neigh.coalition)) {
          val aval = neigh.avaliate(ag)
          aval.onIncrement
            .ofUtility(topUAg)
            .ofWellfare(SimulConfiguration.WELLFARE)
            .ofReciprocalWellfare
            .go({
              topNeigh = neigh; topUAg = aval.uAg
            })
          //          if (aval.uAg > topUAg && aval.wellfare > neigh.coalition.u) {
          //            topNeigh = neigh; topUAg = aval.uAg
          //          }
          testedCoals += neigh.coalition -> aval
          trace iterated (ag, neigh, aval)
        } else {
          trace alreadyIterated (ag, neigh, testedCoals(neigh.coalition))
        }
      })

      if (topUAg > ag.u) {
        ag.abandon()
        val uAgBefore = ag.u
        topNeigh.coligate(ag)
        trace coligate (ag, topNeigh, uAgBefore)
      }
    }) // agent
    showCoalitions(iteration)

    trace finishIteration

    return wellfare
  }

  private def reposition() {
    var position: List[Socialization] = Nil
    val random = new Random()
    position = agents.sort((ag1, ag2) => if (ag1.id < ag2.id) random.nextBoolean() else false)
    agents = position
  }

  def randomize(ags : List[Ag]) : List[Ag] = {
    val random = new Random()
    ags.sort((ag1, ag2) => if (ag1.id < ag2.id) random.nextBoolean() else false)    
  }
  
  private def wellfare = (0.00 /: agents.map((ag) => ag.u))(_ + _) / agents.size

  private def showCoalitions(iteration: Int) {
    val coals = mutable.HashSet.empty[Coalition]
    agents.foreach((ag) => coals += ag.coalition)

    info("Coalitions: " + coals.toString + " - Wellfare[" + wellfare + "]")
  }

  // ------------------------------------------
  // ---------------   CREATE   ---------------

  private def createAgentsUniformDistr(): List[Socialization] = {
    var agents: List[Socialization] = Nil
    for (i <- 1 to SimulConfiguration.AGS) {
      // Randomze resources
      val random = new Random()
      var rcs: List[Int] = Nil
      var rcsPi: List[Int] = Nil
      for (j <- 0 to SimulConfiguration.DIM - 1) {
        if (random.nextDouble() > SimulConfiguration.LACK) {
          rcs = random.nextInt(SimulConfiguration.MAX_RC) :: rcs
          rcsPi = 0 :: rcsPi
        } else {
          rcs = 0 :: rcs
          rcsPi = random.nextInt(SimulConfiguration.MAX_RC) :: rcsPi
        }
      }

      val ag: Ag = new Ag(i, new Rc(rcs)) with Socialization {
        var rcPi = new Rc(rcsPi)
      }

      agents = ag :: agents
    }
    return agents
  }

  private def createAgentsNormalDistr(): List[Socialization] = {
    val mean = SimulConfiguration.MEAN; val variance = SimulConfiguration.VARIANCE
    return lack(
      Socialization(
        Ag(SimulConfiguration.AGS, mean, variance),
        mean * SimulConfiguration.AVALIABILITY, variance))
  }

  private def lack(ags: List[Socialization]): List[Socialization] = {
    //    return ags
    val K = Rc.DIM
    val r_lack = new Random()
    ags.foreach(ag => {
      for (k <- 0 to K - 1) {
        if (r_lack.nextDouble() < SimulConfiguration.LACK)
          ag.rc = Rc(ag.rc, k, 0)
        else
          ag.rcPi = Rc(ag.rcPi, k, 0)
      }
    })
    ags.filter(ag => ag.rcPi.toList.forall(_ == 0)).foreach(ag => {
      val rcs_notnull = ag.rc.toList.zipWithIndex.filter(el => el._1 > 0)
      val k = r_lack.nextInt(rcs_notnull.size)
      ag.rcPi = Rc(ag.rcPi, k, ag.rc(k))
      ag.rc = Rc(ag.rc, k, 0)
    })
    
    ags
  }

}