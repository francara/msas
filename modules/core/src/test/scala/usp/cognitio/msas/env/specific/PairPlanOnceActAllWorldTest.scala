package usp.cognitio.msas.env.specific
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.math.alg.Point
import org.junit.Test
import org.junit.Assert._
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.WorldSense
import usp.cognitio.msas.agent.cog.Plan

class PairPlanOnceActAllWorldTest extends Logging {

  case class World extends PlanOnceActAllWorld(2,5) {
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
          case class PlanAg(world: PlanOnceActAllWorld, _i: Int, _rc: Rc) extends MsasAg(_i, _rc)
          val ag = new PlanAg(this, i, Rc()) with PlanOnceActAllBehaviour {
            override def act(sense: WorldSense) {
                return super.act(sense)
            }
            override def onReplan(plan: Plan) {
            }
          }

          ag.stopWenStucked

          ag.init(this, this)
          enter(ag)
        }
      }
  } // World
  
  @Test
  def manual() {
    val world = World()

    world.populate()
    val ag1 = world.ag(1)
    val ag2 = world.ag(2)

    Point.DIAGONAL = false

    assertEquals(ag1.coalition.members, List(ag1))
    assertEquals(ag2.coalition.members, List(ag2))
    world.act()
    assertEquals(ag1.coalition.members.sort(_.id < _.id), List(ag1, ag2))
    assertEquals(Point(0, 0), world.position(ag1))
    assertEquals(Point(4, 4), world.position(ag2))
    world.act()
    assertEquals(Point(0, 1), world.position(ag1))
    assertEquals(Point(4, 3), world.position(ag2))
    world.act()
    assertEquals(Point(1, 1), world.position(ag1))
    assertEquals(Point(4, 2), world.position(ag2))
    world.act()
    assertEquals(Point(2, 1), world.position(ag1))
    assertEquals(Point(4, 1), world.position(ag2))
    assertFalse(ag1.satisfied)
    assertFalse(ag2.satisfied)
    world.act()
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(3, 1), world.position(ag2))
    assertTrue(ag1.satisfied)
    assertFalse(ag2.satisfied)
    world.act()
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
    world.act()
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
  }

  @Test
  def run() {
    val world = World()

    world.populate()
    val ag1 = world.ag(1)
    val ag2 = world.ag(2)

    assertEquals(ag1.coalition.members, List(ag1))
    assertEquals(ag2.coalition.members, List(ag2))
    while (!ag1.satisfied || !ag2.satisfied) {
      world.act()
    }
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
  }

  /**
   * Pure PlanOnceActAll.
   */
    @Test
  def pure() {
    val world = World()

    world.populate()
    val ag1 = world.ag(1)
    val ag2 = world.ag(2)

    assertEquals(ag1.coalition.members, List(ag1))
    assertEquals(ag2.coalition.members, List(ag2))
    while (!ag1.satisfied || !ag2.satisfied) {
      world.act()
    }
    assertEquals(Point(2, 2), world.position(ag1))
    assertEquals(Point(2, 1), world.position(ag2))
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
  }

}