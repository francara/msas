package usp.cognitio.msas.agent.soc
import usp.cognitio.msas.coal.Coalition
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.kernel.Context
import usp.cognitio.msas.util.Logging

class SocializationContext(f: (List[Ag]) => Coalition) extends Context with Logging {
  var createFunc : (List[Ag]) => Coalition = f
  def createCoalition(ags : List[Ag]) : Coalition = f(ags)
  def createCoalition(ag : Ag) : Coalition = f(List(ag))
}

