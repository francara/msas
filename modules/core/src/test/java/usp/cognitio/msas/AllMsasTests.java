package usp.cognitio.msas;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

@RunWith(Suite.class)
@SuiteClasses({
  usp.cognitio.msas.RcTest.class,
  usp.cognitio.msas.coal.CoalitionTest.class,
  usp.cognitio.msas.coal.VoteCoalitionTest.class,
  usp.cognitio.msas.coal.KVoteCoalitionTest.class,
  usp.cognitio.msas.coal.SocializationTest.class,  
  usp.cognitio.msas.agent.soc.cen.CoalK2Test.class,
  usp.cognitio.msas.env.GridWorldTest.class,  
  usp.cognitio.msas.agent.cog.plan.DPSpaceTest.class,  
  usp.cognitio.msas.agent.cog.AStarTest.class 
})
public class AllMsasTests {

}
