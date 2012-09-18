package usp.cognitio.msas.kernel

class MsasMathObject extends MsasObject {
  protected def round(d: Double) = BigDecimal(d).setScale(2, 
      BigDecimal.RoundingMode.FLOOR).toDouble
}