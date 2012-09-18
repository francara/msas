package usp.cognitio.math;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.analysis.UnivariateFunction;
import org.apache.commons.math3.analysis.function.Gaussian;
import org.apache.commons.math3.analysis.integration.SimpsonIntegrator;
import org.apache.commons.math3.analysis.integration.UnivariateIntegrator;
import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.log4j.Logger;
import org.junit.Test;

public class SimpsonIntegratorTest {
  private static final int MAX_EVAL = 100;
  private static Logger log = Logger.getLogger(SimpsonIntegratorTest.class);

  @Test
  /**
   * Test of integrator for the sine function.
   */
  public void testSinFunction() {
      
      UnivariateFunction f = new Gaussian(10,2);
      UnivariateIntegrator integrator = new SimpsonIntegrator();
      double a, b, expected, tolerance, result;
      
      a = 8; b = 12;
      expected  = 0.68269;
      tolerance = 0.00001;

      tolerance = Math.abs(expected * integrator.getRelativeAccuracy());
      result = integrator.integrate(MAX_EVAL, f, a, b);
      assertEquals(expected, result, tolerance);
      
      log.info("Result: " + result +", tolerance: " + tolerance + " - Relative accuracy: " + integrator.getRelativeAccuracy()
          + " - Absolute accuracy: " + integrator.getAbsoluteAccuracy() 
          + " - Iterations: " + integrator.getIterations());

      NormalDistribution distribution = new NormalDistribution(10, 2);
      result = distribution.cumulativeProbability(a,b);
      log.info("Distribution result: " + result);
      
  }

}
