package usp.cognitio.msas.env.specific
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.Plan
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.math.alg.Point
import org.junit.Test
import org.junit.Assert._

class StuckedWithPlanCompleteActReplanTest {
  case class PlanAg(world: PlanOnceActAllWorld, _i: Int, _rc: Rc) extends MsasAg(_i, _rc) 
  case class World extends PlanOnceActAllWorld(2, 5) {
    override def randRcCell(x: Int, y: Int): Rc = {
      if (x == 1) Rc(0, 1, 0)
      else if (x == 3) Rc(1, 0, 0)
      else Rc.nil
    }
    override def randPosition(ag: MsasAg): (Point, Point) = {
      if (ag.id == 1) (Point(0, 0), Point(2, 2))
      else if (ag.id == 2) (Point(4, 4), Point(2, 1))
      else (Point(0, 0), Point(R / 2, R / 2))
    }
    override def randRcAg(ag: Ag): Rc = {
      if (ag.id == 1) return Rc(1, 0, 0)
      else Rc(0, 1, 0)
    }
    override def enter(ag: MsasAg): GridWorld = {
      val cell = super.enter(ag)
      ag.rc = randRcAg(ag)
      cell
    }
    override def populate() {

      for (i <- 1 to N) {
        /*
           * BEHAVIOUR
           * ---------
           */
        val ag = newAg(this, i)
        ag.stopWenStucked
        ag.init(this, this)
        enter(ag)
      }
    }
    def newAg(world: PlanOnceActAllWorld, id: Int): PlanAg = {
      new PlanAg(world, id, Rc()) with PlanCompleteActReplanBehaviour
    }

  } // World

  @Test
  def testBothStucked() {
    val world = World()
    world.rcs(1)(1) = Rc(0, 1, 1)
    world.rcs(3)(1) = Rc(1, 0, 1)
    world.populate()
    val ag1 = world.ag(1)
    val ag2 = world.ag(2)
    Point.DIAGONAL = false

    assertEquals(0, ag1.qtdReplan)
    assertEquals(0, ag2.qtdReplan)
    
    /*
     * Ag1: stucked at (1,1)
     * Ag2: Stucked at (4,1)
     */
    world.act() // Coligation
    world.act() // ag1 avaliating, ag2 move (4,3)
    
    assertEquals(1, ag1.qtdReplan)
    assertEquals(1, ag2.qtdReplan)    
    
    assertEquals(Point(0, 0), world.position(ag1))
    assertEquals(Point(4, 3), world.position(ag2))
    world.act() // ag1 avaliating, ag2 move (4,2)
    assertEquals(1, ag1.qtdReplan)
    assertEquals(1, ag2.qtdReplan)    
    assertEquals(Point(0, 0), world.position(ag1))
    assertEquals(Point(4, 2), world.position(ag2))
    world.act() // ag1 avaliating, ag2 move (4,1)
    assertEquals(Point(0, 0), world.position(ag1))
    assertEquals(Point(4, 1), world.position(ag2))
    world.act()
    assertEquals(Point(0, 0), world.position(ag1))
    world.act() // Until here, ag1 avaliating coligations.
    assertEquals(Point(0, 0), world.position(ag1))
    world.act() // ag1 stagnated then do the best...
    assertEquals(Point(0, 1), world.position(ag1))
    assertEquals(Point(4, 1), world.position(ag2))

    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    world.act()
    assertEquals(Point(0, 1), world.position(ag1))
    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    world.act()
    assertFalse(ag2.asInstanceOf[PlanCompleteActReplanBehaviour].justReplan)
    assertTrue(ag2.stucked)
    world.act()
    assertTrue(ag2.asInstanceOf[PlanCompleteActReplanBehaviour].justReplan)
    assertFalse(ag2.stucked)
    assertEquals(Point(4, 2), world.position(ag2))    
    world.act()
    assertFalse(ag2.asInstanceOf[PlanCompleteActReplanBehaviour].justReplan)
    assertEquals(Point(3, 2), world.position(ag2))    
    assertFalse(ag1.stucked)
    world.act()
    assertEquals(Point(0, 1), world.position(ag1))
    assertEquals(Point(2, 2), world.position(ag2))
    assertTrue(ag1.stucked)
    assertFalse(ag2.plan.finished)
    assertFalse(ag2.satisfied)
    world.act()
    assertEquals(Point(0, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertTrue(ag1.asInstanceOf[PlanCompleteActReplanBehaviour].justReplan)
    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    assertTrue(ag2.plan.finished)
    assertTrue(ag2.satisfied)
    world.act()
    assertEquals(Point(1, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertFalse(ag1.asInstanceOf[PlanCompleteActReplanBehaviour].justReplan)
    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    assertTrue(ag2.satisfied)
    world.act()
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
    world.act()
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
    world.act()
    world.act()
    world.act()
    world.act()
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertFalse(ag1.stucked)
    assertFalse(ag2.stucked)
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
  }

}