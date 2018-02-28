package transaction

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.testkit.properties.mempool.MempoolTransactionsTest

class BDMempoolTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators
  with MempoolTransactionsTest[Sha256PreimageProposition, BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  override val memPool: BlockchainDevelopersMempool = new BlockchainDevelopersMempool()

  override val transactionGenerator: Gen[BlockchainDevelopersTransaction] = transactionGen

  val nonEmptyTransactionSequenceGenerator: Gen[Seq[BlockchainDevelopersTransaction]] =
    Gen.nonEmptyContainerOf[Seq, BlockchainDevelopersTransaction](transactionGenerator)

  property("size of mempool should increase when adding a non-present tx") {
    forAll(transactionGenerator) { tx =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      mp.get.size shouldEqual 1
    }
  }

  property("size of mempool should not increase when adding a present tx") {
    forAll(transactionGenerator) { tx =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      val mp2 = mp.get.put(tx)
      mp2.isSuccess shouldBe true
      mp2.get.size shouldEqual 1
    }
  }

  property("size of mempool should increase when adding a collection of non-present txs without duplicates (with check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      mp.get.size shouldEqual txs.size
    }
  }

  property("size of mempool should increase for a number of unique non-present txs " +
    "when adding a collection of non-present txs with duplicates (with check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs ++ txs)
      mp.isSuccess shouldBe true
      mp.get.size shouldEqual txs.size
    }
  }

  property("size of mempool should not increase when adding a collection of present txs (with check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      val mp2 = mp.get.put(txs)
      mp2.isSuccess shouldBe true
      mp2.get.size shouldEqual txs.size
    }
  }

  property("size of mempool should increase when adding a collection of non-present txs without duplicates (without check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.putWithoutCheck(txs)
      mp.size shouldEqual txs.size
    }
  }

  property("size of mempool should increase for a number of unique non-present txs " +
    "when adding a collection of non-present txs with duplicates (without check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.putWithoutCheck(txs ++ txs)
      mp.size shouldEqual txs.size
    }
  }

  property("size of mempool should not increase when adding a collection of present txs (without check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.putWithoutCheck(txs)
      val mp2 = mp.putWithoutCheck(txs)
      mp2.size shouldEqual txs.size
    }
  }

  property("size of mempool should decrease when removing a present tx") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      val mp2 = mp.get.remove(txs.head)
      mp2.size shouldBe txs.size - 1
    }
  }

  property("size of mempool should not decrease when removing non-present tx") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs, tx) =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      val mp2 = mp.get.remove(tx)
      mp2.size shouldBe txs.size
    }
  }

  property("mempool txs should be filtered successfully") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      val mp2 = mp.get.filter(tx => tx equals txs.head)
      mp2.size shouldBe 1
    }
  }

  property("existing tx should be obtained by id") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      val txOpt = mp.get.getById(txs.head.id)
      txOpt.nonEmpty shouldBe true
    }
  }

  property("non-existing tx should not be obtained by id") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs, tx) =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      val txOpt = mp.get.getById(tx.id)
      txOpt.nonEmpty shouldBe false
    }
  }

  property("mempool should contain present tx") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      mp.get.contains(txs.head.id) shouldBe true
    }
  }

  property("mempool should not contain non-present tx") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs, tx) =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      mp.get.contains(tx.id) shouldBe false
    }
  }

  property("present txs should be obtained by their ids") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs, tx) =>
      val mp = memPool.put(txs :+ tx)
      mp.isSuccess shouldBe true
      mp.get.getAll(txs.map(_.id)) sameElements txs
    }
  }

  property("non-present txs should not be obtained by their ids") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs, tx) =>
      val mp = memPool.put(tx)
      mp.isSuccess shouldBe true
      mp.get.getAll(txs.map(_.id)).size shouldBe 0
    }
  }

  property("required number of txs should be taken") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs, tx) =>
        val mp = memPool.put(txs :+ tx)
        mp.isSuccess shouldBe true
        mp.get.take(txs.size).size shouldBe txs.size
    }
  }

  property("maximum number of txs that can be taken should equals mempool size") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs =>
      val mp = memPool.put(txs)
      mp.isSuccess shouldBe true
      mp.get.take(txs.size + 1).size shouldBe mp.get.size
    }
  }
}