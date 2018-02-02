package mining

import scorex.crypto.hash.CryptographicHash32

import scala.math.BigInt
import java.security.SecureRandom

import com.google.common.primitives.Ints

class PoWMiner[HF <: CryptographicHash32](hashFunction: HF) {

  private val MaxTarget: BigInt = BigInt(1, Array.fill(32)((-1).toByte))

  def doWork(data: Array[Byte], difficulty: BigInt): ProvedData = {
    val seed: Array[Byte] = SecureRandom.getInstanceStrong.generateSeed(32)

    def loop(hash: Array[Byte]): Array[Byte] = {
      val newHash = hashFunction.hash(hash ++ data)
      if (MaxTarget / BigInt(1, newHash) >= difficulty) hash
      else loop(newHash)
    }

    ProvedData(data, Ints.fromByteArray(loop(seed)))
  }

  def validateWork(data: ProvedData, difficulty: BigInt): Boolean = realDifficulty(data) >= difficulty

  private def realDifficulty(noncedData: ProvedData): BigInt =
    MaxTarget / BigInt(1, hashFunction.hash(noncedData.bytes))

}
