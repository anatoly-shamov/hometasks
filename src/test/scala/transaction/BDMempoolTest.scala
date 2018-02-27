package transaction

import io.circe.Json
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.ModifierId
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.MessageToSign
import scorex.testkit.generators.CoreGenerators
import scorex.testkit.properties.mempool.MempoolTransactionsTest

class BDMempoolTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with CoreGenerators
  with MempoolTransactionsTest[Sha256PreimageProposition, BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override val memPool: BlockchainDevelopersMempool = new BlockchainDevelopersMempool()
  override val transactionGenerator: Gen[BlockchainDevelopersTransaction] = for {
    inputsGen <- Gen.nonEmptyContainerOf[Array, OutputId](
      Gen.oneOf(
        OutputId @@ Array[Byte](0, 1, 2, 3),
        OutputId @@ Array[Byte](0, 1, 2, 4),
        OutputId @@ Array[Byte](0, 1, 2, 5),
        OutputId @@ Array[Byte](0, 1, 2, 6),
        OutputId @@ Array[Byte](0, 1, 2, 7)
      )
    )
    outputsGen <- Gen.nonEmptyContainerOf[Array, (Sha256PreimageProposition, Value)](
      Gen.oneOf(
        (Sha256PreimageProposition(Digest32 @@ Array[Byte](0, 2, 4, 2)), Value @@ 0L),
        (Sha256PreimageProposition(Digest32 @@ Array[Byte](0, 2, 4, 3)), Value @@ 1L),
        (Sha256PreimageProposition(Digest32 @@ Array[Byte](0, 2, 4, 4)), Value @@ 2L),
        (Sha256PreimageProposition(Digest32 @@ Array[Byte](0, 2, 4, 5)), Value @@ 3L),
        (Sha256PreimageProposition(Digest32 @@ Array[Byte](0, 2, 4, 6)), Value @@ 4L)
      )
    )
    signaturesGen <- Gen.nonEmptyContainerOf[Array, Sha256PreimageProof](
      Gen.oneOf(
        Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0, 1, 1, 0)),
        Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0, 1, 1, 1)),
        Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0, 1, 1, 2)),
        Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0, 1, 1, 3)),
        Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0, 1, 1, 4))
      )
    )
  } yield BlockchainDevelopersTransaction(inputsGen, outputsGen, signaturesGen)

  class MalformedTransaction
    extends BlockchainDevelopersTransaction(
      Array[OutputId](OutputId @@ Array[Byte](0)),
      Array[(Sha256PreimageProposition, Value)]((Sha256PreimageProposition(Digest32 @@ Array[Byte](0)), Value @@ 42L)),
      Array[Sha256PreimageProof](Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0)))
    ) {
    override lazy val id: ModifierId = ModifierId @@ Array[Byte](0)
  }

  property("size of mempool should increase when adding a single tx") {
    forAll(transactionGenerator) { tx =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      mp.get.size shouldEqual 1
    }
  }

  property("size of mempool should increase when adding a collection of txs") {
    forAll(transactionGenerator, transactionGenerator) { (tx1, tx2) =>
      whenever(!(tx1.id sameElements tx2.id)) {
        val mp = memPool.put(Array[BlockchainDevelopersTransaction](tx1, tx2))
        mp.isSuccess shouldBe true
        mp.get.size shouldEqual 2
      }
    }
  }

  property("adding malformed tx with check should be unsuccessful") {
    val tx = new MalformedTransaction
    var mp = memPool.put(tx)
    mp.isSuccess shouldBe false
  }

  property("adding malformed tx without check should be successful") {
    val tx = new MalformedTransaction
    var mp = memPool.putWithoutCheck(Array[BlockchainDevelopersTransaction](tx, tx))
    mp.size shouldBe 1
  }

  property("adding existing txs without check should be successful") {
    forAll(transactionGenerator) { tx =>
      var mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      var mp2 = mp.get.putWithoutCheck(Array[BlockchainDevelopersTransaction](tx, tx))
      mp2.size shouldBe 1
    }
  }

  property("size of mempool should decrease when removing existing tx") {
    forAll(transactionGenerator) { tx =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      val mp2 = mp.get.remove(tx)
      mp2.size shouldBe 0
    }
  }

  property("size of mempool should not decrease when removing non-existing tx") {
    forAll(transactionGenerator, transactionGenerator) { (tx1, tx2) =>
      whenever(!(tx1.id sameElements tx2.id)) {
        val mp = memPool.put(tx1)
        mp.isSuccess shouldBe true
        val mp2 = mp.get.remove(tx2)
        mp2.size shouldBe 1
      }
    }
  }

  property("mempool txs should be filtered") {
    forAll(transactionGenerator, transactionGenerator) { (tx1, tx2) =>
      whenever(!(tx1.id sameElements tx2.id)) {
        val mp = memPool.put(Array[BlockchainDevelopersTransaction](tx1, tx2))
        mp.isSuccess shouldBe true
        val mp2 = mp.get.filter(tx => tx.id equals tx1.id)
        mp2.size shouldBe 1
      }
    }
  }

  property("existing tx should be obtained by id") {
    forAll(transactionGenerator) { tx =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      val getTx = mp.get.getById(tx.id)
      getTx.isEmpty shouldBe false
    }
  }

  property("non-existing tx shouldn't be obtained by id") {
    forAll(transactionGenerator, transactionGenerator) { (tx1, tx2) =>
      whenever(!(tx1.id sameElements tx2.id)) {
        val mp = memPool.put(tx1)
        mp.isSuccess shouldBe true
        val getTx = mp.get.getById(tx2.id)
        getTx.isEmpty shouldBe true
      }
    }
  }

  property("mempool should contain existing tx") {
    forAll(transactionGenerator) { tx =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      mp.get.contains(tx.id) shouldBe true
    }
  }

  property("mempool shouldn't contain non-existing tx") {
    forAll(transactionGenerator, transactionGenerator) { (tx1, tx2) =>
      whenever(!(tx1.id sameElements tx2.id)) {
        val mp = memPool.put(tx1)
        mp.isSuccess shouldBe true
        mp.get.contains(tx2.id) shouldBe false
      }
    }
  }

  property("existing txs should be obtained by getAll") {
    forAll(transactionGenerator, transactionGenerator, transactionGenerator) { (tx1, tx2, tx3) =>
      whenever(!(tx1.id sameElements tx2.id) && !(tx1.id sameElements tx3.id) && !(tx3.id sameElements tx2.id)) {
        val allTxs = Array[BlockchainDevelopersTransaction](tx1, tx2, tx3)
        val selectedTxs = Array[BlockchainDevelopersTransaction](tx1, tx2)
        val selectedIds = Array[ModifierId](tx1.id, tx2.id)
        val mp = memPool.put(allTxs)
        mp.isSuccess shouldBe true
        mp.get.getAll(selectedIds) sameElements selectedTxs
      }
    }
  }

  property("required number of txs should be taken") {
    forAll(transactionGenerator, transactionGenerator, transactionGenerator) { (tx1, tx2, tx3) =>
      whenever(!(tx1.id sameElements tx2.id) && !(tx1.id sameElements tx3.id) && !(tx3.id sameElements tx2.id)) {
        val allTxs = Array[BlockchainDevelopersTransaction](tx1, tx2, tx3)
        val mp = memPool.put(allTxs)
        mp.isSuccess shouldBe true
        mp.get.take(2).size shouldBe 2
        mp.get.take(4).size shouldBe 3
      }
    }
  }
}