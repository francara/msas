package usp.cognitio.msas.simul

import org.junit.Assert.assertEquals
import org.junit.Test

import usp.cognitio.math.alg.Point
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.agent.MsasAg
import usp.cognitio.msas.env.GridWorld
import usp.cognitio.msas.env.SessionSoc
import usp.cognitio.msas.util.Logging
import usp.cognitio.msas.Rc

class GridWorldSimulTest extends Logging {

  @Test
  def runAg1() {
    val world = new GridWorld(5)
    val ag1: MsasAg = new MsasAg(1, Rc(10, 0, 5)) {
      rcPi = Rc()
    }
    val ag2: MsasAg = new MsasAg(2, Rc(0, 10, 5)) {
      rcPi = Rc()
    }

    ag1.init(world, world)
    ag2.init(world, world)

    for (i <- 0 to world.R - 1) world.rcs(1)(i) = Rc(0, 0, 0)
    world.enter(ag1, 0, 0)
    world.enter(ag2, 3, 3)

    /*
     * The agent is able to move until the target.
     */
    val sense = world.sense(ag1)
    ag1.act(sense)
    assertEquals(Point(1, 0), world.whereIs(ag1).point)
    ag1.act(sense.refresh)
    assertEquals(Point(1, 1), world.whereIs(ag1).point)
    ag1.act(sense.refresh)
    assertEquals(Point(2, 1), world.whereIs(ag1).point)
    ag1.act(sense.refresh)
    assertEquals(Point(2, 2), world.whereIs(ag1).point)
    ag1.act(sense.refresh)
    assertEquals(Point(2, 2), world.whereIs(ag1).point)
  }

  @Test
  def runIteration() {

    val world = new GridWorld(5) {
      override def randRcCell(x: Int, y: Int): Rc = {
        Rc.nil
      }
      override def randRcAg(ag: Ag): Rc = {
        Rc.nil
      }
      override def randPosition(ag: MsasAg): (Point, Point) = {
        if (ag.id == 1) (Point(0, 0), Point(2,2))
        else if (ag.id == 2) (Point(4,4), Point(2,1))
        else (Point(0, 0), Point(R / 2, R / 2))
      }
      override def communicate(who: MsasAg, neigh: MsasAg): SessionSoc = 
        SessionSoc(this, who, neigh, coals(neigh))
    }
    
    val ag1: MsasAg = new MsasAg(1, Rc(10, 0, 5)) {
      rcPi = Rc()
    }
    val ag2: MsasAg = new MsasAg(2, Rc(0, 10, 5)) {
      rcPi = Rc()
    }

    ag1.init(world, world)
    ag2.init(world, world)

    for (i <- 0 to world.R - 1) world.rcs(1)(i) = Rc(0, 0, 0)
    world.enter(ag1, 0, 0)
    world.enter(ag2, 3, 3)

    /*
     * The agent is able to move until the target.
     */
    val sense = world.sense(ag1)

  }

}