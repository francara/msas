package usp.cognitio.msas.agent
import usp.cognitio.msas.env.WorldSense
import org.apache.log4j.Logger

trait Traceable

trait MindTraceable extends Traceable {
  private val logger = Logger.getLogger("usp.cognitio.mind")
  
  def ag : MsasAg
  
  def tracePhy() = logger.trace(pre(ag.rc.toString + "/" + ag.rcPi.toString) + "[Rc^-:" + ag.rcMinus + "]")
  
  def info(sense: WorldSense, action:String, msg:String) = logger.info(pre(sense, action, msg))
  def debug(sense: WorldSense, action:String, msg:String) = logger.debug(pre(sense, action, msg))
  def trace(sense: WorldSense, action:String, msg:String) = logger.trace(pre(sense, action, msg))
  private def pre(sense: WorldSense, action:String, msg:String) : String =
    "[IT:" + sense.it + "]" + pre("<" + action + "> " + msg)
  private def pre(msg:String) = "[AG:" + ag + "] " + msg
}

trait CoalTraceable extends Traceable {
  private val logger = Logger.getLogger("usp.cognitio.coalition")
 
  
}

trait SocTraceable extends Traceable {
  private val logger = Logger.getLogger("usp.cognitio.social")
  
}