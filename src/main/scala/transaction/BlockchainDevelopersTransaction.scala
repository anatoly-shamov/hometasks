package transaction

import com.google.common.primitives.Bytes
import io.circe._
import io.circe.syntax._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import supertagged.untag
import scorex.core.utils.concatBytes

import scala.util.Try

case class BlockchainDevelopersTransaction(inputs: IndexedSeq[OutputId],
                                           outputs: IndexedSeq[(Sha256PreimageProposition, Value)],
                                           signatures: IndexedSeq[Sha256PreimageProof]
                                          ) extends Transaction[Sha256PreimageProposition] {

  override type M = BlockchainDevelopersTransaction

  private def seqToBytes[A](sequence: IndexedSeq[A], mapping: A => Array[Byte]): Array[Byte] =
    if (sequence.nonEmpty) concatBytes(sequence.map(mapping)) else Array[Byte]()

  override val messageToSign: Array[Byte] =
    Bytes.concat(
      seqToBytes[OutputId](
        inputs,
        i => untag(i)),
      seqToBytes[(Sha256PreimageProposition, Value)](
        outputs,
        o => o._1.serializer.toBytes(o._1)),
      seqToBytes[Sha256PreimageProof](
        signatures,
        s => untag(s.preimage))
    )

  override def serializer: Serializer[BlockchainDevelopersTransaction] = BCTransactionSerializer

  implicit val modifierTypeIdEncoder: Encoder[ModifierTypeId] = (a: ModifierTypeId) => {
    untag(a).asJson
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = (a: ModifierId) => {
    untag(a).asJson
  }

  implicit val outputIdEncoder: Encoder[OutputId] = (a: OutputId) => {
    untag(a).asJson
  }

  implicit val propositionEncoder: Encoder[Sha256PreimageProposition] = (a: Sha256PreimageProposition) => {
    Json.obj("hash" -> untag(a.hash).asJson)
  }

  implicit val proofEncoder: Encoder[Sha256PreimageProof] = (a: Sha256PreimageProof) => {
    Json.obj("preimage" -> untag(a.preimage).asJson)
  }

  implicit val valueEncoder: Encoder[Value] = (a: Value) => {
    untag(a).asJson
  }

  implicit val txEncoder: Encoder[BlockchainDevelopersTransaction] = (a: BlockchainDevelopersTransaction) => {
    Json.obj(
      "modifierTypeId" -> a.modifierTypeId.asJson,
      "id" -> a.id.asJson,
      "inputs" -> a.inputs.asJson,
      "outputs" -> a.outputs.asJson,
      "signatures" -> a.signatures.asJson
    )
  }

  override def json: Json =
    this.asJson
}

object BCTransactionSerializer extends Serializer[BlockchainDevelopersTransaction] {
  override def toBytes(obj: BlockchainDevelopersTransaction): Array[Byte] =
    ???

  override def parseBytes(bytes: Array[Byte]): Try[BlockchainDevelopersTransaction] =
    ???
}