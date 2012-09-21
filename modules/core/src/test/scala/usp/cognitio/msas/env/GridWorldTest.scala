package usp.cognitio.msas.env
import usp.cognitio.msas.util.Logging
import org.junit.Test
import org.junit.Assert._
import usp.cognitio.msas.agent.Ag
import usp.cognitio.msas.env.Direction.East
import usp.cognitio.msas.env.Direction.West
import usp.cognitio.msas.env.Direction.North
import usp.cognitio.msas.env.Direction.South
import usp.cognitio.msas.Rc
import usp.cognitio.msas.agent.MsasAg

class GridWorldTest extends Logging {
  @Test
  def testRc {
    debug("*********   Test RC   *********")
    val world = new GridWorld(4)
    /*
     * Initialize the world.
     */
    world.putRc(1, 1, new Rc(3 :: 0 :: 0 :: Nil))
    debug("Cell 1,1: " + world(1, 1))

    assertEquals(world(1, 1).rc, new Rc(List(3, 0, 0)))

    /*
     * Insert agent into world.
     */
    val ag1: MsasAg = new MsasAg(1, Rc.nil)
    ag1.init(world,world)
    world.enter(ag1, 1, 1)

    debug("Cell 1,1: Entered agent 1: " + world(1, 1))
    assertEquals(world(1, 1).ags, List(ag1))

    val ag2: MsasAg = new MsasAg(2, Rc.nil)
    ag2.init(world,world)
    world.enter(ag2, 1, 1)

    debug("Cell 1,1: Entered agent 2: " + world(1, 1))
    assertEquals(world(1, 1).ags, List(ag2, ag1))
  }

  @Test
  def testIntegration {
    debug("*********   Test INTEGRATION   *********")
    val world = new GridWorld(4)
    /*
     * Initialize the world.
     */
    world.putRc(1, 1, new Rc(3 :: 0 :: 0 :: Nil))
    world.putRc(2, 1, new Rc(0 :: 1 :: 0 :: Nil))
    val (ag1, ag2) = (new MsasAg(1, Rc.nil), new MsasAg(2, Rc.nil))
    ag1.init(world,world)
    ag2.init(world,world)
    world.enter(ag1, 1, 1).enter(ag2, 1, 1)

    assertTrue(world(1, 1).contains(ag1))
    assertTrue(world(1, 1).contains(ag2))
    assertFalse(world(2, 1).contains(ag1))
    assertFalse(world(2, 1).contains(ag2))

    assertEquals(world.next(1, 1, East), world(2, 1))
    world.moveTo(ag2, East)

    assertTrue(world(1, 1).contains(ag1))
    assertFalse(world(1, 1).contains(ag2))
    assertFalse(world(2, 1).contains(ag1))
    assertTrue(world(2, 1).contains(ag2))

    debug("Cell 1,1: agent 1: " + world(1, 1))
    debug("Cell 1,2: agent 2: " + world(2, 1))

    // Path
    assertEquals(world.toRc((1, 1) :: (2, 1) :: Nil), new Rc(3 :: 1 :: 0 :: Nil))


  }

  @Test
  def testMove {
    debug("*********   Test MOVE   *********")
    val world = new GridWorld(4)
    val ag = new MsasAg(1, Rc.nil)
    ag.init(world, world)
    world.enter(ag, 0, 0)

    assertIn(world, ag, (0, 0))
    world.moveTo(ag, North)
    assertIn(world, ag, (0, 1))
    world.moveTo(ag, East)
    assertIn(world, ag, (1, 1))
    world.moveTo(ag, West)
    assertIn(world, ag, (0, 1))
    world.moveTo(ag, East)
    assertIn(world, ag, (1, 1))
    world.moveTo(ag, East)
    assertIn(world, ag, (2, 1))
    world.moveTo(ag, North)
    assertIn(world, ag, (2, 2))
    world.moveTo(ag, North)
    assertIn(world, ag, (2, 3))

    // Moviment in the edge
    world.moveTo(ag, North)
    assertIn(world, ag, (2, 3))
    world.moveTo(ag, North)
    assertIn(world, ag, (2, 3))
    world.moveTo(ag, East)
    assertIn(world, ag, (3, 3))
    world.moveTo(ag, East)
    assertIn(world, ag, (3, 3))
  }

  private def assertIn(world: GridWorld, ag: Ag, cell: (Int, Int)): Unit = assertIn(world, List(ag), cell)
  private def assertIn(world: GridWorld, ags: List[Ag], cell: (Int, Int)): Unit = {
    ags.foreach(ag => {
      for (i <- 0 to world.R - 1)
        for (j <- 0 to world.R - 1)
          if ((i, j) == cell) assertTrue(world(i, j).contains(ag))
          else assertFalse(world(i, j).contains(ag))
    })
  }

}