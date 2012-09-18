package usp.cognitio.msas.simul
import usp.cognitio.msas.util.Logging
import org.junit.Test
import java.util.regex.Pattern
import usp.cognitio.msas.simul.compartilhamento.SimulConfiguration

class SimulConfigurationTest extends Logging {

  @Test
  def testRegexp() {
    val str = "[DIM:123.12][OUTRO:987]"
//    val pattern = Pattern.compile("(\\[\\w*:\\d*\\])*")
//    val pattern = Pattern.compile("(\\[(\\w+):(\\d+\\.?\\d*)\\])")
//    val pattern = Pattern.compile("\\[\\w+:\\d+\\.?\\d*\\]")
    val pattern = Pattern.compile("\\]")
    val grpattern = Pattern.compile("\\[(\\w+):(\\d+\\.?\\d*)")
    val split = pattern.split(str)
    debug("Splits:" + split.toList)
    split.toList.foreach(el => {
      val matcher = grpattern.matcher(el)
      debug("Groups:" + matcher.groupCount())
      debug("Matches:" + matcher.matches())
      debug("Group 1:" + matcher.group(1) + " Group 2: " + matcher.group(2))
      val (variable, value) = (matcher.group(1), matcher.group(2).toDouble)
    })

    val agstr = "AG:1:Rc{[1,2]}Pi{[3,4]}"
    val agentterm = Pattern.compile("\\[?AG:(\\d+):Rc\\{\\[(.*)\\]\\}.*Pi\\{\\[(.*)\\]\\}\\]?")
    val agmatcher = agentterm.matcher(agstr)
    debug("AG matches" + agmatcher.matches())
    debug("AG:" + agmatcher.group(1) + " Rc:: " + agmatcher.group(2) + "Pi: " + agmatcher.group(3))
  }
  
  @Test
  def testLines {
    SimulConfiguration.read("src/main/resources/msas-config.simul")
    var hasNext = SimulConfiguration.hasNext() 
    while (hasNext) {
      SimulConfiguration.next
      val iterations3 = SimulConfiguration.ITERATIONS_3
      debug("[ITERATIONS_3:" + iterations3 + "]")
      hasNext = SimulConfiguration.hasNext() 
      debug("hasNext:" + hasNext)
    }    
  }
  
}
