package usp.cognitio.msas.simul.log

class RichLogString(var msg:String) {
  def *(other : String) : String = msg + " " + other
  def *|(other : String) : String = msg + " [" + other + "]"
  def *|(tt:String, other : Object) : String = msg + " [" + tt + ":" + other.toString + "]"
  def *|(tt:String, other : Double) : String = msg + " [" + tt + ":" + other.toString + "]"
  def **(other : String) : String = msg + " *** " + other
  def ***(other : String) : String = msg + " ****** " + other

}