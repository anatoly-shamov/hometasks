package mining

import org.scalameter.api._
import org.scalameter.{Context, Gen}
import scorex.crypto.hash.Blake2b256

class PoWMinerBench extends Bench.LocalTime {

  override def defaultConfig: Context = Context(
    exec.minWarmupRuns -> 10,
    exec.maxWarmupRuns -> 20,
    exec.independentSamples -> 1,
    exec.benchRuns -> 200,
    verbose -> true
  )

  override def measurer: Measurer[Double] = new Measurer.IgnoringGC

  val miner = new PoWMiner(Blake2b256)

  val dataSizes = Gen.exponential("size")(1 * 1024, 1 * 1024, 4)
  val dataArrays = for (dataSize <- dataSizes) yield Array.fill[Byte](dataSize)(0)

  val difficulties = Gen.exponential("difficulty")(1 * 1024, 1 * 1024, 4)

  val parallelismLevels = Gen.range("parallelismLevel")(1, 4, 1)

  val inputs = for {
    difficulty <- difficulties
    data <- dataArrays
    parallelismLevel <- parallelismLevels
  } yield (data, difficulty, parallelismLevel)

  performance of "PoWMiner" in {
    measure method "doWorkPar" in {
      using(inputs) in { input =>
        miner.doWorkPar(input._1, input._2, input._3)
      }
    }
  }
}