package usp.cognitio.msas.agent.cog
import org.junit.Test
import org.junit.Assert._
import usp.cognitio.msas.agent.ActPhy
import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.ActSoc

class PlanTest {

  @Test
  def testNext() {
    val plan = Plan(ActPhy(Point(0, 0)) :: ActPhy(Point(1, 1)) :: ActPhy(Point(2, 2)) :: Nil)
    var action = plan.action
    assertEquals(ActPhy(Point(0, 0)), action)
    assertEquals(0, plan.index)

    plan.next
    action = plan.action
    assertEquals(ActPhy(Point(1, 1)), action)
    assertEquals(1, plan.index)
  }

  @Test
  def testAddCurrent() {
    val plan = Plan(ActPhy(Point(0, 0)) :: ActPhy(Point(1, 1)) :: ActPhy(Point(2, 2)) :: Nil)
    plan.addCurrent(ActSoc())
    var action = plan.action
    assertEquals(ActSoc(), action)
    assertEquals(0, plan.index)

    plan.next
    action = plan.action
    assertEquals(ActPhy(Point(0, 0)), action)
    assertEquals(1, plan.index)

    plan.next
    action = plan.action
    assertEquals(ActPhy(Point(1, 1)), action)
    assertEquals(2, plan.index)

    plan.addCurrent(ActSoc())
    action = plan.action
    assertEquals(ActSoc(), action)
    assertEquals(2, plan.index)

    plan.next
    action = plan.action
    assertEquals(ActPhy(Point(1, 1)), action)
    assertEquals(3, plan.index)

    plan.next
    action = plan.action
    assertEquals(ActPhy(Point(2, 2)), action)
    assertEquals(4, plan.index)

    assertFalse(plan.next)
    action = plan.action
    assertEquals(ActPhy(Point(2, 2)), action)
    assertEquals(4, plan.index)
  }

  @Test
  def testRemaining() {
    val plan = Plan(ActPhy(Point(0, 0)) :: ActPhy(Point(1, 1)) :: ActPhy(Point(2, 2)) :: Nil)
    val r1 = plan.remaining
    assertEquals(plan, r1)
    
    plan.next
    plan.next
    var action = plan.action
    assertEquals(ActPhy(Point(2, 2)), action)
    assertEquals(2, plan.index)
    val r2 = plan.remaining
    assertEquals(Plan(ActPhy(Point(2,2)) :: Nil), r2)

    
    
  }

}