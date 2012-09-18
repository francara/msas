package usp.cognitio.msas.simul.compartilhamento
import java.util.regex.Pattern

import scala.io.Source

import usp.cognitio.msas.util.Logging

class FileSimulConfiguration(fileName: String) extends Logging {

  var lines = Source.fromFile(fileName).getLines().toList
  def next = {
    while(lines != Nil && Pattern.matches("^#.*$", lines.head)) {
      lines = lines.tail
    }
    
    val line = lines.head
    lines = lines.tail
    parseVariables(line)

    var nextLine = if (lines.length > 0) lines.head else ""
    while (nextLine.matches("^\\W*\\[AG:.*")) {
      lines = lines.tail
      parseAg(nextLine)

      nextLine = if (lines.length > 0) lines.head else ""
    }
    
    while(lines != Nil && Pattern.matches("^ *$", lines.head)) {
      lines = lines.tail
    }
    
  }

  def parseVariables(line: String) {
    val pattern = Pattern.compile("^\\[|(\\]\\[)|\\]$")
    val valueterm = Pattern.compile("\\[?(\\w+):(\\d+\\.?\\d*)")

    val split = pattern.split(line)
    debug("Splits:" + split.toList)

    split.toList.filter(_ != "").foreach(el => {
      var matcher = valueterm.matcher(el)
      val vlterm = matcher.matches()
      if (vlterm) {
        val (variable, value) = (matcher.group(1), matcher.group(2).toDouble)
        parseValueTerm(variable, value)
      }

    })
  }

  def parseAg(line: String) {
    val agentterm = Pattern.compile(".*\\[?AG:(\\d+):Rc\\{\\[(.*)\\]\\}.*Pi\\{\\[(.*)\\]\\}\\]?.*")
    val matcher = agentterm.matcher(line)
    val agterm = matcher.matches()
    if (agterm) {
      val (id, rcs, pis) = (matcher.group(1).toLong, matcher.group(2), matcher.group(3))
      parseAgTerm(id, rcs, pis)
    }
  }

  def parseValueTerm(variable: String, value: Double) {
    debug("Variable:" + variable + " Value: " + value)
    if (variable == "DIM") SimulConfiguration.DIM = value.asInstanceOf[Int]
    if (variable == "AGS") SimulConfiguration.AGS = value.asInstanceOf[Int]
    if (variable == "MAX_RC") SimulConfiguration.MAX_RC = value.asInstanceOf[Int]
    if (variable == "AVALIABILITY") SimulConfiguration.AVALIABILITY = value
    if (variable == "LACK") SimulConfiguration.LACK = value
    if (variable == "MEAN") SimulConfiguration.MEAN = value
    if (variable == "VARIANCE") SimulConfiguration.VARIANCE = value
    if (variable == "ITERATIONS") SimulConfiguration.ITERATIONS = value.asInstanceOf[Int]
    if (variable == "ITERATIONS_3") SimulConfiguration.ITERATIONS_3 = value.asInstanceOf[Int]    
    if (variable == "REDISTRIBUTE") SimulConfiguration.REDISTRIBUTE = if (value.asInstanceOf[Int] == 0) false else true
    if (variable == "WELLFARE") SimulConfiguration.WELLFARE = if (value.asInstanceOf[Int] == 0) false else true
  }

  def parseAgTerm(id: Long, strcs: String, stpis: String) {
    debug("AG:" + id + " Rcs: " + strcs + " Pis: " + stpis)
    var rcs: List[Int] = Nil
    var pis: List[Int] = Nil

    strcs.split(",").foreach(r => rcs = r.toInt :: rcs)
    stpis.split(",").foreach(r => pis = r.toInt :: pis)

    SimulConfiguration.rcs += (id -> rcs.reverse)
    SimulConfiguration.rcPis += (id -> pis.reverse)
  }

}

object SimulConfiguration {
  var DIM = 5
  var AGS = 7
  var MAX_RC = 10

  var TOTAL_RC = AGS * MAX_RC
  var AVALIABILITY = 0.8
  var TOTAL_PI_RC = (TOTAL_RC * AVALIABILITY).asInstanceOf[Int]
  /** Max percentual of dimensions with lack of resources. */
  var LACK = 0.3

  var MEAN: Double = 5
  var VARIANCE: Double = 3

  var ITERATIONS = 10
  var ITERATIONS_3 = 10

  var REDISTRIBUTE = false
  var WELLFARE = false

  var rcs: scala.collection.Map[Long, List[Int]] = scala.collection.mutable.HashMap.empty[Long, List[Int]]
  var rcPis: scala.collection.Map[Long, List[Int]] = scala.collection.mutable.HashMap.empty[Long, List[Int]]

  var configuration: FileSimulConfiguration = null

  def read(fileName: String) {
    configuration = new FileSimulConfiguration(fileName)
  }

  def hasNext() = configuration != null &&  configuration.lines != Nil
  def next = configuration.next

  override def toString(): String = {
    ""
  }

  def toConfig(): List[String] = {
    var variables = "[DIM:%d]" format DIM
    variables += "[AGS:%d]" format AGS
    variables += "[MAX_RC:%d]" format MAX_RC
    variables += "[AVALIABILITY:%s]" format AVALIABILITY.toString
    variables += "[LACK:%s]" format LACK
    variables += "[MEAN:%s]" format MEAN
    variables += "[VARIANCE:%s]" format VARIANCE
    variables += "[ITERATIONS:%d]" format ITERATIONS
    variables += "[ITERATIONS_3:%d]" format ITERATIONS_3
    variables += "[REDISTRIBUTE:%d]" format (if (REDISTRIBUTE) 1 else 0)
    variables += "[WELLFARE:%d]" format (if (WELLFARE) 1 else 0)

    var ags : List[String] = Nil
    val agIds = rcs.keys.toList
    agIds.foreach(id => {
      val rc = rcs(id); val rcPi = rcPis(id)
      ags = "[AG:%s:Rc{%s}Pi{%s}]".format(id, rc.mkString("[",",","]"), rcPi.mkString("[",",","]")) :: ags
    })
    
    variables :: ags
  }

}
