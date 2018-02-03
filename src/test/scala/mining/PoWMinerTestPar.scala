package mining

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import scorex.testkit.generators.CoreGenerators

class PoWMinerTestPar extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with CoreGenerators {

  property("should generate valid proofs in parallel (4)") {
    val miner = new PoWMiner(Blake2b256)
    val parallelismLevel = 4
    forAll(smallInt, nonEmptyBytesGen) { (difficulty, data) =>
      val proved = miner.doWorkPar(data, difficulty, parallelismLevel)
      proved.data shouldEqual data
      miner.validateWork(proved, difficulty) shouldBe true
    }
  }
}