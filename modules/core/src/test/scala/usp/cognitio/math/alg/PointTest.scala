package usp.cognitio.math.alg

import org.junit.Assert._
import org.junit.Test

class PointTest {

  @Test
  def testNeigh() {
    Point.DIAGONAL = true
    val point = Point(0,0)
    
    assertEquals(List((0,1), (1,0), (1,1)), point.neighs.map(_.xy))
  }
  
}