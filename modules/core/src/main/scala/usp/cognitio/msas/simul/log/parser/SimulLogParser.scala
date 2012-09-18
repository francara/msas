package usp.cognitio.msas.simul.log.parser
import scala.io.Source
import java.util.regex.Pattern
import org.apache.log4j.Logger
import java.nio.charset.Charset
import java.io.BufferedWriter
import java.io.FileWriter

object SimulLogParser {
  val logger: Logger = Logger.getLogger(getClass().getName())
  val lines = Source.fromFile("/Users/frank/dev/research/msas/log/msas-simul.log").getLines

  val row = scala.collection.mutable.Map.empty[Int, Map[String, Double]]
  /*
   * Parse lines
   */
  var n_row = 0; var startRow = true
  lines.foreach(line => {
    if (startRow) {
      n_row += 1
      parseStart(n_row, line)
    } else {
      parseFinish(n_row, line)
    }
    startRow = !startRow
  })

  var outputStream = new BufferedWriter(new FileWriter("/Users/frank/dev/research/msas/log/msas-simul.csv"));
  row.keySet.toList.sort(_ < _).foreach(i => {
    logger.info("Linha :" + i + " " + row(i))
    val linha = row(i)
    outputStream.write(linha("Ags").toString + "," 
        + linha("Wellfare")/linha("EnvWellfare") + "," + linha("Sigma") + ","
        + linha("Wellfare") + "," + linha("EnvWellfare")
        + "\n")
  })
  outputStream.close()
  

  def parseStart(i: Int, line: String) {
    val rowMatcher = Pattern.compile(".*START SIMUL.*AGS:(\\d+).*").matcher(line)
    if (!rowMatcher.matches()) throw new UnsupportedOperationException("")
    val n_ag = rowMatcher.group(1).toDouble
    row(i) = Map("Ags" -> n_ag)
    logger.debug("AGS: " + n_ag)
  }

  def parseFinish(i: Int, line: String) {
    val rowMatcher = Pattern.compile(".*FINISH SIMUL.*Wellfare:(\\d+\\.?\\d*).*Sigma:(\\d+\\.?\\d*).*EnvWellfare:(\\d+\\.?\\d*).*").matcher(line)
    if (!rowMatcher.matches()) throw new UnsupportedOperationException("")
    val n_wellfare = rowMatcher.group(1).toDouble
    val n_sigma = rowMatcher.group(2).toDouble
    val n_envwell = rowMatcher.group(3).toDouble

    row(i) += ("Wellfare" -> n_wellfare)
    row(i) += ("Sigma" -> n_sigma)
    row(i) += ("EnvWellfare" -> n_envwell)

    logger.debug("WELLFARE: " + n_wellfare / n_envwell + " SIGMA: " + n_sigma)
  }

  /*
   * Generate output
   */
  def main(args: Array[String]) {

  }

}