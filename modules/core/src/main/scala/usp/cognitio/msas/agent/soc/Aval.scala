package usp.cognitio.msas.agent.soc
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc

class Aval(val who:Socialization, val neigh:Socialization, val alocs:scala.collection.Map[Ag,Rc], val wellfare:Double) {
  var before : Double = 0
  
  def uAg = who.u(alocs(who))
  def _uNeigh = neigh.u(alocs(neigh))
  
  def onIncrement = new IncrementAval(this)
}

class IncrementAval(val aval:Aval) {
  private var lastOf = true
  
  def ofUtility(refU : Double) : IncrementAval = {lastOf = lastOf && aval.uAg > refU; this}
  def ofWellfare : IncrementAval = {lastOf = lastOf && aval.wellfare > aval.neigh.coalition.u; this}
  def ofWellfare(doWellfare:Boolean) : IncrementAval = {
    lastOf = 
      if (doWellfare) lastOf && aval.wellfare > aval.neigh.coalition.u
      else lastOf
    this
  }
  def ofReciprocalWellfare : IncrementAval = {
    /*
     * Avaliation excluding 'who' in the coalition with and without 'who'
     * Delata[U_S] > -Delta[U_i]
     */
    lastOf = 
      lastOf && aval.neigh.coalition.u(aval.alocs) - aval.neigh.coalition.u > -1*(aval.who.u(aval.alocs(aval.who)) - aval.who.u)
    this
  }
  def go(f: Unit) {if (lastOf) f; aval}
}
