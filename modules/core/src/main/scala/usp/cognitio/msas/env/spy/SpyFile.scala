package usp.cognitio.msas.env.spy
import java.io.File
import scala.collection.Map
import scala.io.BufferedSource
import scala.io.Source
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc

class SpyFile(file: File) {
  val writer = new PrintWriter(file)
  // Current action
  var index : Int = 1
  
  def moving(ag:Ag, pos: Point) {
	  write("IDX:" + index + " AG:" + ag + " MOVE:" + pos)
  }
  
  def sensing(who: Ag) {
	  write("IDX:" + index + " AG:" + who + " SENSE")    
  }

  def avaliating(who: Ag, neigh: Ag, alocs: Map[Ag,Rc]) {
	  write("IDX:" + index + " AG:" + who + " AVALIATE NEIGH:" + neigh)    
  }
  
  def coligating(who: Ag, neigh: Ag, alocs: Map[Ag,Rc]) {
	  write("IDX:" + index + " AG:" + who + " COLIGATE NEIGH:" + neigh)    
  }
  
  def write(row: String) {
    writer.println(row)
    index += 1
  }
 
  def close() {
    writer.close()
  }
  
}

object SpyFile {
  private val idFileName = "../core/src/main/resources/simul/msas.spy"
  private var session = 0
  
  next()
    
  def next() = {    
    session = Source.fromFile(idFileName).mkString.toInt
    
    // Increments file
    val writer = new BufferedWriter(new FileWriter(idFileName));
    writer.write((session + 1).toString)
    writer.close()    
  }
  
  def apply() = {
    new SpyFile(
      new File("../core/src/main/resources/simul/phy-" + session + ".simul")
    )
  }
}