package usp.cognitio.msas.env
import usp.cognitio.msas.agent.Ag

trait WorldPhy {
  val N:Int
  def move(ag:Ag, direcion: Int) : Boolean
  
  def sense(ag:Ag) : WorldSense
}