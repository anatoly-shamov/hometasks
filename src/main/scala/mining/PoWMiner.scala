package mining

import scorex.crypto.hash.CryptographicHash32

import scala.math.BigInt
import java.security.SecureRandom.getInstanceStrong

import com.google.common.primitives.Ints

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

class PoWMiner[HF <: CryptographicHash32](hashFunction: HF) {

  private val MaxTarget: BigInt = BigInt(1, Array.fill(32)((-1).toByte))

  def doWork(data: Array[Byte], difficulty: BigInt): ProvedData = {
    def getRandomBytes = Ints.toByteArray(getInstanceStrong.nextInt())
    def loop(hash: Array[Byte]): Array[Byte] = {
      if (MaxTarget / BigInt(1, hashFunction.hash(hash ++ data)) >= difficulty) hash
      else loop(getRandomBytes)
    }
    ProvedData(data, Ints.fromByteArray(loop(getRandomBytes)))
  }

  def doWorkPar(data: Array[Byte], difficulty: BigInt, parallelismLevel: Int): ProvedData = {
    def task = Future {
      doWork(data, difficulty)
    }
    Await.result(Future.firstCompletedOf(Array.fill(parallelismLevel)(task)), Duration.Inf)
  }

  def validateWork(data: ProvedData, difficulty: BigInt): Boolean = realDifficulty(data) >= difficulty

  private def realDifficulty(noncedData: ProvedData): BigInt =
    MaxTarget / BigInt(1, hashFunction.hash(noncedData.bytes))

}
