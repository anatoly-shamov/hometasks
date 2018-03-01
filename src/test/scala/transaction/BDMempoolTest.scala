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

  property("Size of mempool should increase when adding a non-present transaction") {
    forAll(transactionGenerator) { tx: BlockchainDevelopersTransaction =>
      val m: BlockchainDevelopersMempool = memPool.put(tx).get
      m.size shouldEqual 1
    }
  }

  property("Size of mempool should not increase when adding a present transaction") {
    forAll(transactionGenerator) { tx: BlockchainDevelopersTransaction =>
      val m: BlockchainDevelopersMempool = memPool.put(tx).get
      val m2: BlockchainDevelopersMempool = m.put(tx).get
      m2.size shouldEqual 1
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (with check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present txs with duplicates (with check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs ++ txs).get
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should not increase when adding a collection of present transactions (with check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      val m2: BlockchainDevelopersMempool = m.put(txs).get
      m2.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase when adding a collection of non-present transactions " +
    "without duplicates (without check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.putWithoutCheck(txs)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should increase for a number of unique non-present transactions " +
    "when adding a collection of non-present transactions with duplicates (without check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.putWithoutCheck(txs ++ txs)
      m.size shouldEqual txs.size
    }
  }

  property("Size of mempool should not increase when adding a collection of present transactions (without check)") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.putWithoutCheck(txs)
      val m2: BlockchainDevelopersMempool = m.putWithoutCheck(txs)
      m2.size shouldEqual txs.size
    }
  }

  property("Size of mempool should decrease when removing a present transaction") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      val m2: BlockchainDevelopersMempool = m.remove(txs.head)
      m2.size shouldBe txs.size - 1
    }
  }

  property("Size of mempool should not decrease when removing a non-present transaction") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs: Seq[BlockchainDevelopersTransaction], tx: BlockchainDevelopersTransaction) =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      val m2: BlockchainDevelopersMempool = m.remove(tx)
      m2.size shouldBe txs.size
    }
  }

  property("Mempool transactions should be filtered successfully") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      val m2: BlockchainDevelopersMempool = m.filter(tx => tx equals txs.head)
      m2.size shouldBe 1
    }
  }

  property("Present transactions should be available by id") {
    forAll(transactionGenerator) { tx: BlockchainDevelopersTransaction =>
      val m: BlockchainDevelopersMempool = memPool.put(tx).get
      m.getById(tx.id).isDefined shouldBe true
    }
  }

  property("Non-present transactions should not be available by id") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs: Seq[BlockchainDevelopersTransaction], tx: BlockchainDevelopersTransaction) =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      m.getById(tx.id).isDefined shouldBe false
    }
  }

  property("Mempool should contain present transactions") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      m.contains(txs.head.id) shouldBe true
    }
  }

  property("Mempool should not contain non-present transactions") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs: Seq[BlockchainDevelopersTransaction], tx: BlockchainDevelopersTransaction) =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      m.contains(tx.id) shouldBe false
    }
  }

  property("Present transactions should be obtained by their ids") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs: Seq[BlockchainDevelopersTransaction], tx: BlockchainDevelopersTransaction) =>
      val m: BlockchainDevelopersMempool = memPool.put(txs :+ tx).get
      m.getAll(txs.map(_.id)) sameElements txs
    }
  }

  property("Non-present transactions should not be obtained by their ids") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs: Seq[BlockchainDevelopersTransaction], tx: BlockchainDevelopersTransaction) =>
      val m: BlockchainDevelopersMempool = memPool.put(tx).get
      m.getAll(txs.map(_.id)).size shouldBe 0
    }
  }

  property("Required number of transactions should be taken from mempool") {
    forAll(nonEmptyTransactionSequenceGenerator, transactionGenerator) { (txs: Seq[BlockchainDevelopersTransaction], tx: BlockchainDevelopersTransaction) =>
      val m: BlockchainDevelopersMempool = memPool.put(txs :+ tx).get
      m.take(txs.size).size shouldBe txs.size
    }
  }

  property("Maximum number of transactions that can be taken should equals mempool size") {
    forAll(nonEmptyTransactionSequenceGenerator) { txs: Seq[BlockchainDevelopersTransaction] =>
      val m: BlockchainDevelopersMempool = memPool.put(txs).get
      m.take(txs.size + 1).size shouldBe m.size
    }
  }
}