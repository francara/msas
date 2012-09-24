package usp.cognitio.msas.env
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.agent.Ag
import usp.cognitio.math.alg.Point
import org.junit.Test
import org.junit.Assert._
import usp.cognitio.msas.env.specific.PlanOnceActAllBehaviour

class RandomGridWorldTest extends Logging {

  /**
   * Each row around the target has a color.
   */
  @Test
  def defined() {
    Rc.DIM = 3
    val world = new GridWorld(5) {
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

      /**
       * Randomize position, source and target.
       */
      def enter(ag: MsasAg): GridWorld = {
        val (start, target) = this.randPosition(ag)
        ag.target = target
        enter(ag, start.x, start.y)
      }

      def act() {
        ags.foreach(ag => {
          val sense = this.sense(ag)
          ag.act(sense)
        })
      }
    }

    val ag1: MsasAg = new MsasAg(1, Rc(1, 0, 0)) with PlanOnceActAllBehaviour {
      rcPi = Rc(0, 0, 0)
    }
    val ag2: MsasAg = new MsasAg(2, Rc(0, 1, 0)) with PlanOnceActAllBehaviour {
      rcPi = Rc(1, 0, 0)
    }

    ag1.init(world, world)
    ag2.init(world, world)

    world.enter(ag1)
    world.enter(ag2)

    assertEquals(ag1.coalition.members, List(ag1))
    assertEquals(ag2.coalition.members, List(ag2))
    world.act()
    assertEquals(ag1.coalition.members, List(ag1,ag2))
    assertEquals(Point(0,0), world.position(ag1))
    assertEquals(Point(4,4), world.position(ag2))
    world.act()
    assertEquals(Point(0,1), world.position(ag1))
    assertEquals(Point(4,3), world.position(ag2))
    world.act()
    assertEquals(Point(1,1), world.position(ag1))
    assertEquals(Point(4,2), world.position(ag2))
    world.act()
    assertEquals(Point(2,1), world.position(ag1))
    assertEquals(Point(3,2), world.position(ag2))
    assertFalse(ag1.satisfied)
    assertFalse(ag2.satisfied)
    world.act()
    assertEquals(Point(2,2), world.position(ag1))
    assertEquals(Point(3,1), world.position(ag2))
    assertTrue(ag1.satisfied)
    assertFalse(ag2.satisfied)
    world.act()
    assertEquals(Point(2,2), world.position(ag1))
    assertEquals(Point(2,1), world.position(ag2))
    assertTrue(ag1.satisfied)
    assertTrue(ag2.satisfied)
    world.act()
    assertEquals(Point(2,2), world.position(ag1))
    assertEquals(Point(2,1), world.position(ag2))
  }

}