package transaction

import scorex.core.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.crypto.hash.Blake2b256
import supertagged._

import scala.collection.immutable.HashMap
import scala.util.Try

class BlockchainDevelopersMempool(val state: Map[ModifierId, BlockchainDevelopersTransaction])
  extends MemoryPool[BlockchainDevelopersTransaction, BlockchainDevelopersMempool] {

  def this() {
    this(HashMap.empty)
  }

  case class MalformedTxException(message: String) extends Exception(message)

  override def put(tx: BlockchainDevelopersTransaction): Try[BlockchainDevelopersMempool] = Try {
    if (!(untag(tx.id) sameElements Blake2b256(tx.messageToSign)))
      throw MalformedTxException(s"Transaction ${tx.id} is not valid")
    new BlockchainDevelopersMempool(state + (tx.id -> tx))
  }

  override def put(txs: Iterable[BlockchainDevelopersTransaction]): Try[BlockchainDevelopersMempool] = {
    if (txs.isEmpty) Try(this)
    else put(txs.head).get.put(txs.tail)
  }

  override def putWithoutCheck(txs: Iterable[BlockchainDevelopersTransaction]): BlockchainDevelopersMempool = {
    if (txs.isEmpty) this
    else new BlockchainDevelopersMempool(state + (txs.head.id -> txs.head))
      .putWithoutCheck(txs.tail)
  }

  override def remove(tx: BlockchainDevelopersTransaction): BlockchainDevelopersMempool =
    new BlockchainDevelopersMempool(state - tx.id)

  override def filter(condition: BlockchainDevelopersTransaction => Boolean): BlockchainDevelopersMempool =
    new BlockchainDevelopersMempool(state.filter { case (id, tx) => condition(tx) })

  override def getById(id: ModifierId): Option[BlockchainDevelopersTransaction] =
    state.get(id)

  override def contains(id: ModifierId): Boolean =
    state.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[BlockchainDevelopersTransaction] = for {
    id <- ids
  } yield getById(id).get

  override def size: Int =
    state.size

  override def take(limit: Int): Iterable[BlockchainDevelopersTransaction] =
    state.values.take(limit)

  override type NVCT = BlockchainDevelopersMempool
}
