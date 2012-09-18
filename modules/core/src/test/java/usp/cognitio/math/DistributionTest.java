package usp.cognitio.math;

import java.util.ArrayList;
import java.util.Arrays;

import org.apache.commons.math3.random.RandomData;
import org.apache.commons.math3.random.RandomDataImpl;
import org.apache.log4j.Logger;
import org.junit.Test;

public class DistributionTest {

  private static Logger log = Logger.getLogger(DistributionTest.class);
  
  @Test
  public void test() {
    Double[] value = new Double[100];
    RandomData randomData = new RandomDataImpl(); 
    for (int i = 0; i < 100; i++) {
        value[i] = randomData.nextGaussian(10, 2);
    }
    log.debug("Vaores: " + new ArrayList<Double>( Arrays.asList(value)) );
    
    
    
    
//    BigDecimal(d).setScale(2, 
//        BigDecimal.RoundingMode.FLOOR).toDouble
    
  }

}
