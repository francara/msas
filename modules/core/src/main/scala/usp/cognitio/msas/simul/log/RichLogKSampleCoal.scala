package usp.cognitio.msas.simul.log
import usp.cognitio.msas.coal.KSampleVoteCoalition
import usp.cognitio.msas.coal.KLinearSampleCoalitionGame

class RichLogKSampleCoal(var coal: KSampleVoteCoalition) {
  def toKEsh(X: Int, k: Int): String = {
    val k_mean = coal.mean(k) * X
    val sigma = Math.sqrt(coal.mean(k))
    val k_sigma = Math.sqrt(coal.mean(k)/X)

    return ("[X:" + X + "]" *| ("k",k) *|("quota(k)", coal.quota(k))
        *| ("Mean", coal.mean(k)) *| ("NormMean", k_mean)
        *| ("Variance", coal.variance(k)) 
        *| ("Sigma", sigma) *| ("NormSigma", k_sigma))
  }

  implicit def StringToLog(msg:String) : RichLogString = new RichLogString(msg)

}

class RichLogKLinearSampleCoal(var coal: KLinearSampleCoalitionGame) {
  def toKEsh(X: Int, k: Int): String = {
    val k_mean = coal.mean(k) * X
    val sigma = Math.sqrt(coal.mean(k))
    val k_sigma = Math.sqrt(coal.mean(k)/X)

    return ("[X:" + X + "]" *| ("k",k) *|("quota(k)", coal.quota(k))
        *| ("Mean", coal.mean(k)) *| ("NormMean", k_mean)
        *| ("Variance", coal.variance(k)) 
        *| ("Sigma", sigma) *| ("NormSigma", k_sigma))
  }

  implicit def StringToLog(msg:String) : RichLogString = new RichLogString(msg)

}