package usp.cognitio.msas.util
import usp.cognitio.msas.agent.Ag
import scala.collection.mutable.ListBuffer

class Permute(pAgs : List[Ag]) {
  val ags : List[Ag] = pAgs
  
  def go : List[List[Ag]] = doPerm(null, ags)
  
  private def doPerm(pivot : Ag, work : List[Ag]) : List[List[Ag]] = {
    if (work.length == 0) return List(pivot :: Nil)
    
    val a : ListBuffer[List[Ag]] = new ListBuffer[List[Ag]]()
    work.foreach( head => {
      doPerm(head, work.remove(_ == head))
        .foreach(a.append(_))
    })

    if (pivot == null) return a.toList
    
    val b : ListBuffer[List[Ag]] = new ListBuffer[List[Ag]]()
    a.foreach(permut => b.append(pivot :: permut))
    
    return b.toList
  }
  
}

