package usp.cognitio.math
import org.junit.Test
import org.apache.commons.math3.random.RandomData
import org.apache.commons.math3.random.RandomDataImpl
import usp.cognitio.msas.util.Logging
import scala.collection.mutable.ArrayBuffer

class WeightDistributionTest extends Roundable with Logging {

  @Test
  def testWeight() {
    val value:Array[Double] = new Array[Double](100)
    val randomData : RandomData = new RandomDataImpl() 
    for (i <- 0 to 99) {
        value(i) = randomData.nextGaussian(15, 0.000001);
    }
    debug("Valores: \t\t" + value.map((el) => el.toString).toList );
    
    val rdvalue:Array[Int] = new Array[Int](100)
    var i=0
    value.foreach((v) => {rdvalue(i) = value(i).asInstanceOf[Int]; i+=1})
    debug("Valores truncados: \t" + rdvalue.map((el) => el.toString).toList );

    val dist = ArrayBuffer.fill(30)(0)
    debug("Distribuicao")
    rdvalue.foreach((v) => dist(v) = dist(v) + 1)
    debug("Distribuicao: \t" + dist.toList );
    debug("Distribuicao: \t" + dist.toList.zipWithIndex.map((el) => (el._2,el._1).toString ));

    
    
  }
  
}