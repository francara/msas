package usp.cognitio.msas

import org.junit.Assert._
import org.junit.Test
import usp.cognitio.msas.util.Logging

class RcTest extends Logging {

  @Test
  def testPlus() {
    val rc = Rc(1,2,3)
    val rcPlus = rc^+ Rc(1,1,1)
    
    assertEquals(rcPlus, Rc(0,1,2))
  }
  
  @Test
  def testNil() {
    val rc0 = Rc.zero(2)
    val rc1 = Rc(1,2)
    
    val res = rc0 + rc1
    val res_res = res + res
    
    assertEquals(res, new Rc(List(1,2)))
    assertEquals(res_res, new Rc(List(2,4)))    
  }
  
  @Test
  def testGe() {
    val rc1 = Rc(1,2)
    val rc2 = Rc(1,3)
    val rc3 = Rc(0,3)
    
    assertTrue(rc2 >= rc1)
    assertFalse(rc1 >= rc2)
    assertTrue(rc1 >= rc1)
    assertFalse(rc1 >= rc3)
    assertTrue(rc2 >= rc3)
  }
  
  
}