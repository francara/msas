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
  usp.cognitio.msas.agent.cog.PlanTest.class,  
  usp.cognitio.msas.agent.cog.AStarTest.class, 
  usp.cognitio.msas.agent.EgoSocTest.class,
  usp.cognitio.msas.agent.EgoCogTest.class,
  usp.cognitio.msas.env.RandomGridWorldTest.class,
  usp.cognitio.msas.env.specific.PairPlanOnceActAllWorldTest.class,
  usp.cognitio.msas.env.specific.StuckedWithPlanCompleteActAllTest.class,
  usp.cognitio.msas.env.specific.StuckedWithPlanCompleteActReplanTest.class
})
public class AllMsasTests {

}
