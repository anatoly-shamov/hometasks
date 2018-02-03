package mining

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import scorex.testkit.generators.CoreGenerators

class PoWMinerTestOther extends PropSpec
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

  property("should generate valid proofs with case class") {
    val miner = new PoWMiner(Blake2b256)
    forAll(smallInt, nonEmptyBytesGen) { (difficulty, data) =>
      val proved = miner.doWorkCase(data, difficulty)
      proved.data shouldEqual data
      miner.validateWork(proved, difficulty) shouldBe true
    }
  }

}