package usp.cognitio.math

trait Roundable {
  // TODO Optimize - Rounding.
  def round(d: Double) = BigDecimal(d).setScale(2, 
      BigDecimal.RoundingMode.FLOOR).toDouble
  def roundUp(d: Double) = BigDecimal(d).setScale(2, 
      BigDecimal.RoundingMode.HALF_UP).toDouble
  def roundUpInt(d: Double) = BigDecimal(d).setScale(0, 
      BigDecimal.RoundingMode.HALF_UP).toInt      
}