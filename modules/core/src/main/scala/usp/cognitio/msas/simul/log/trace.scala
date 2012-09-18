package usp.cognitio.msas.simul.log
import org.apache.log4j.Logger
import org.apache.log4j.RollingFileAppender
import usp.cognitio.msas.agent.soc.Socialization.agToSocialization
import usp.cognitio.msas.agent.soc.Aval
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.soc.Socialization
import usp.cognitio.msas.agent.Ag
import usp.cognitio.math.Roundable
import usp.cognitio.msas.simul.compartilhamento.SimulConfiguration
import scala.math.min
import org.apache.commons.math3.stat.StatUtils
import java.io.BufferedWriter
import java.io.FileWriter
import usp.cognitio.msas.Rc

object trace extends Roundable {
  private val simul = Logger.getLogger("usp.cognitio.simul");
  private val social = Logger.getLogger("usp.cognitio.social");
  private val mind: Logger = Logger.getLogger("usp.cognitio.mind")
  private val coalition: Logger = Logger.getLogger("usp.cognitio.coalition")

  private var simulNum : Int = 0
  private var agents : List[Ag] = null
  private var iteration = 1
  
  def start(agents : List[Ag]) {
    this.agents = agents
    this.simulNum += 1
    
    mind.info("*********   STARTING SIMULATION " + simulNum + "   *********")
    social.info("*********   STARTING SIMULATION " + simulNum + "   *********")
    coalition.info("*********   STARTING SIMULATION " + simulNum + "   *********")
    
    var s_config = SimulConfiguration.toConfig()
    social.info(s_config(0))
    s_config.tail.foreach(line => social.info(line))
    
    var wellfare:Double = 0
    agents.foreach((ag) => {
      mind.info(toId(ag) * "Starting" * toU(ag))
      social.info("Starting" * toId(ag) * toU(ag) * "Rc:" * ag.rc.toString * "Pi:" * ag.rcPi.toString)
      wellfare += ag.u
    })
    social.info("[STARTING]" ** "Initial ags avg wellfare" * toWellfare)

    var agmatrix = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-agent.csv", true));
    agents.foreach((ag) => {
      agmatrix.write(simulNum + ",Rc," + ag.id + "," + ag.rc.toList.mkString(",") + "\n")      
    })
    agents.foreach((ag) => {
      agmatrix.write(simulNum + ",RcPi," + ag.id + "," + ag.rcPi.toList.mkString(",") + "\n")      
    })
    agmatrix.close()

    var totalRc = 0; var totalPi = 0
    coalitions().foreach(coal => {
      totalRc += coal.rc.sum
      totalPi += coal.rcPi.sum
    })
    
    simul.info(toIt ** "START SIMUL" * s_config.toString() * toWellfare() * toEnvWellfare())    
  }
  
  def iteration(index: Int) {
    iteration = index
    mind.info("***   ITERATION: " + iteration + " ***")
    social.info("***   ITERATION: " + iteration + " ***")
    coalition.info("***   ITERATION: " + iteration + " ***")

    var totalRc : Rc = Rc.zero()
    coalitions().foreach(coal => {
      coalition.info(toIt * toId(coal) * toMind(coal))
      totalRc = totalRc + coal.rc
    })
    coalition.info(toIt ** "Total Rc" * totalRc.toString)
    
    social.debug(toIt ** "Environment" *| ("EnvWellfare", envWellfare()))
  }
  
  def init(ag: Ag) {
    social.debug(toIt * toId(ag) ** "Aval" * toU(ag) )
    mind.debug(toIt * toId(ag) ** "Aval" * toU(ag) * toMind(ag) )
  }

  
  def iterated(ag: Ag, neigh: Ag, aval: Aval) {
    social.debug(toId(ag) * "Aval" *| ("Neigh", neigh) *| ("U", aval.uAg.toString))
  }

  def alreadyIterated(ag: Ag, neigh: Ag, aval: Aval) {
    social.debug(toId(ag) * "Aval" * "(reaproveitada)" *| ("Neigh", neigh) *| ("U", aval.uAg.toString))
  }
    
  def iteratedWell(ag: Ag, neigh: Ag, aval: Aval) {
    social.debug(toId(ag) * "Aval Wellfare" *| ("Neigh", neigh) *| ("U", aval.uAg.toString))
  }
    
  def aval(neigh: Socialization, who: Socialization) {
    mind.debug(toIt * toId(who) * "Aval" *| ("Neigh", neigh) * toId(neigh.coalition) * toMind(neigh.coalition) ) 
  }

  def aval(who: Socialization, neigh1:Socialization, neigh2:Socialization) {
    mind.debug(toIt * toId(who) * "Aval" *| ("Neigh_1", neigh1) *| ("Neigh_2", neigh2) * toId(who.coalition) * toMind(who.coalition) ) 
  }
    
  def coligate(ag: Ag, neigh: Ag, beforeWellfare:Double) {
    social.debug(toIt * toId(ag) ** "Coligation" *| ("Neigh", neigh) *| ("U_before", beforeWellfare)  * toU(ag) )
  }

  def coligate(ag: Ag, neigh1: Ag, neigh2:Ag, beforeWellfare:Double) {
    social.debug(toIt * toId(ag) ** "Coligation" *| ("Neigh_1", neigh1) *| ("Neigh_2", neigh2) *| ("U_before", beforeWellfare)  * toU(ag) )
  }

  // **************************************
  // ************   Wellfare   ************
  def envWellfare() : Double = {
    val allRc = (Rc.zero() /: coalitions().map(_.rc)) (_ + _)
    val allRcPi = (Rc.zero() /: coalitions().map(_.rcPi)) (_ + _)
    
    // Creates an agent with all resources.
    val env = new Ag(9999, allRc) with Socialization {
      var rcPi = allRcPi
    }
    
    return env.u
  }
  
  def detailedWellfare() = {
    agents.foreach(ag => {
      social.trace(toId(ag) *| ("U", ag.u))
    })
  }
  
  // ***************************************
  // ************   Coalition   ************
  
  def alocate(coal:Coalition) = { 
    coalition.info(toIt * toId(coal) * toMind(coal))
    if (coalition.isTraceEnabled()) {
      coal.members.foreach(ag => {
        coalition.trace(toIt * toId(coal) * "\t" *| ("AG", ag) * toMind(ag) )
      })
    }
  }
  
  def shapley(coal:Coalition, vls:List[(Ag,Double)]) {
    coalition.debug(toIt * toId(coal) * "Shapley" * vls.mkString(","))
  }
  
  def division(coal:Coalition, div:List[(Ag, Int)]) {
    coalition.debug(toIt * toId(coal) * "Division" * div.mkString(","))    
  }
  
  def alocation(coal:Coalition, als:List[(Ag, Rc)]) {
    coalition.debug(toIt * toId(coal) * "Alocation" * als.mkString(","))        
  }

  def available(coal:Coalition, rc:Rc) {
    coalition.debug(toIt * toId(coal) * "Available" * rc.toString)            
  }

  def alocationAndAvailable(coal:Coalition, als:List[(Ag, Rc)]) {
    coalition.debug(toIt * toId(coal) * "Final Alocation" * als.mkString(","))            
  }
  
  def alocationRest(coal:Coalition, als:List[(Ag, Rc)]) {
    coalition.debug(toIt * toId(coal) * "Rest" * als.mkString(","))            
  }
    
  def availableAfterRest(coal:Coalition, rc:Rc) {
    coalition.debug(toIt * toId(coal) * "After rest" * rc.toString)            
  }
  
  def finishIteration {
    val us:Array[Double] = new Array[Double](agents.size)
    val l_us = agents.map(_.u)
    for (i <- 0 to agents.size-1) us(i) = l_us(i)
    
    val mean = agents.map(_.u).sum / agents.size
    val variance = StatUtils.variance(us, mean)
    val sigma = Math.sqrt(variance)
    
    social.info(toIt ** "Ags avg wellfare" * coalitions.toString * toWellfare() *|("Sigma", sigma))
    
    // Normalized wellfare
    // Removes th 10% worst agents and the 10% better agents
    val percent = min(1,(SimulConfiguration.AGS * 0.1).asInstanceOf[Int])
    var normags = agents.sort(_.u < _.u)
    for (i <- 0 to percent) normags = normags.tail
    normags = normags.reverse
    for (i <- 0 to percent) normags = normags.tail

    
    detailedWellfare()
  }
  
  def finish {
    val us:Array[Double] = new Array[Double](agents.size)
    val l_us = agents.map(_.u)
    for (i <- 0 to agents.size-1) us(i) = l_us(i)
    
    val mean = agents.map(_.u).sum / agents.size
    val variance = StatUtils.variance(us, mean)
    val sigma = Math.sqrt(variance)
    
    var well:Double = 0; agents.foreach(well += _.u)
    well = round(well/agents.size)

    simul.info(toIt ** "FINISH SIMUL" * coalitions.toString * toWellfare() *|("Sigma", sigma) * toEnvWellfare())
    
    val coals = coalitions
    val coalsize = coals.toList.map(_.members.size)
    var coalsizeMp = scala.collection.immutable.SortedMap.empty[Int,Int]
    for(i <- 1 to 40) coalsizeMp += (i -> 0) 
    coalsize.foreach(size => coalsizeMp += (size -> (coalsizeMp(size) + 1)))
    val sizestr = coalsizeMp.map(_._2).mkString(",")
    var sizecsv = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-size.csv", true));
    sizecsv.write(
        simulNum + ","
        + SimulConfiguration.AVALIABILITY + ","
        + SimulConfiguration.LACK + ","
        + SimulConfiguration.AGS + ","
        + SimulConfiguration.MEAN + ","
        + SimulConfiguration.VARIANCE + ","
        + SimulConfiguration.ITERATIONS + ","
        + coalsizeMp.map(_._2).mkString(",")
        + "\n"
    )
    sizecsv.close()    
    
    var simulcsv = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-simul.csv", true));
    simulcsv.write(
        simulNum + ","
        + SimulConfiguration.AVALIABILITY + ","
        + SimulConfiguration.LACK + ","
        + SimulConfiguration.AGS + ","
        + SimulConfiguration.MEAN + ","
        + SimulConfiguration.VARIANCE + ","
        + SimulConfiguration.ITERATIONS + ","
        + well + ","
        + sigma + ","
        + envWellfare() 
        + "\n"
    )
    simulcsv.close()
    
  }
  
  def rollover() {
    social.getAppender("SOCIAL").asInstanceOf[RollingFileAppender].rollOver()
    mind.getAppender("MIND").asInstanceOf[RollingFileAppender].rollOver()
    coalition.getAppender("COALITION").asInstanceOf[RollingFileAppender].rollOver()    
  }

  def coalitions() : scala.collection.Set[Coalition] = {
    val coals = scala.collection.mutable.HashSet.empty[Coalition]
    agents.foreach(ag => {
      coals += ag.coalition
    })
    return coals
  }
  
  // **************************************
  // ************   Language   ************
  
  def toIt = "[IT:"+ this.iteration +"]"
  def toId(ag:Ag) = "[AG:" + ag.id + "]"
  def toId(coalition:Coalition) = "[COAL:" + coalition.members.mkString(",") + "]"
  def toU(ag : Ag) = "[U:" + ag.u + "]"
  def toWellfare() = {
    var well:Double = 0; agents.foreach(well += _.u)
    "[Wellfare:" + round(well/agents.size) + "]"
  }
  
  def toEnvWellfare() = "[EnvWellfare:" + envWellfare() + "]"
  
  def toMind(ag: Socialization) = "Rc{" + ag.rc.toString + "} / Pi{" + ag.rcPi.toString + "}"
  def toMind(coalition:Coalition): String = "Rc{" + coalition.rc.toString + "} / Pi{" + coalition.rcPi.toString + "}"

  implicit def StringToLog(msg:String) : RichLogString = new RichLogString(msg)

}

//class StringLog(var msg:String) {
//  def *(other : String) : String = msg + " " + other
//  def *|(other : String) : String = msg + " [" + other + "]"
//  def *|(tt:String, other : Object) : String = msg + " [" + tt + ":" + other.toString + "]"
//  def *|(tt:String, other : Double) : String = msg + " [" + tt + ":" + other.toString + "]"
//  def **(other : String) : String = msg + " *** " + other
//  def ***(other : String) : String = msg + " ****** " + other
//}