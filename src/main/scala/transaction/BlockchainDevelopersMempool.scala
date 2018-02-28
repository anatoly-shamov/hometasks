package transaction

import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool

import scala.collection.immutable.HashMap
import scala.util.Try

class BlockchainDevelopersMempool(val state: Map[ModifierId, BlockchainDevelopersTransaction])
  extends MemoryPool[BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  def this() {
    this(HashMap.empty)
  }

  override def put(tx: BlockchainDevelopersTransaction): Try[BlockchainDevelopersMempool] = put(Seq(tx))

  override def put(txs: Iterable[BlockchainDevelopersTransaction]): Try[BlockchainDevelopersMempool] = Try {
    txs.foreach(tx => require(tx.inputs.length == tx.signatures.length))
    putWithoutCheck(txs)
  }

  override def putWithoutCheck(txs: Iterable[BlockchainDevelopersTransaction]): BlockchainDevelopersMempool = {
    if (txs.isEmpty) this
    else new BlockchainDevelopersMempool(state + (txs.head.id -> txs.head)).putWithoutCheck(txs.tail)
  }

  override def remove(tx: BlockchainDevelopersTransaction): BlockchainDevelopersMempool =
    new BlockchainDevelopersMempool(state - tx.id)

  override def filter(condition: BlockchainDevelopersTransaction => Boolean): BlockchainDevelopersMempool =
    new BlockchainDevelopersMempool(state.filter { case (id, tx) => condition(tx) })

  override def getById(id: ModifierId): Option[BlockchainDevelopersTransaction] =
    state.get(id)

  override def contains(id: ModifierId): Boolean =
    state.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[BlockchainDevelopersTransaction] = (for {
    id <- ids
  } yield state.get(id)) flatten

  override def size: Int =
    state.size

  override def take(limit: Int): Iterable[BlockchainDevelopersTransaction] =
    state.values.take(limit)

  override type NVCT = BlockchainDevelopersMempool
}
