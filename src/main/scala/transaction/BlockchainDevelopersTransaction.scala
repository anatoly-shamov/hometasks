package transaction

import io.circe.Json
import io.circe.syntax.EncoderOps
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.crypto.hash.Digest32

import scala.util.Try

case class BlockchainDevelopersTransaction(inputs: IndexedSeq[OutputId],
                                           outputs: IndexedSeq[(Sha256PreimageProposition, Value)],
                                           signatures: IndexedSeq[Sha256PreimageProof]
                                          ) extends Transaction[Sha256PreimageProposition] {
  override type M = BlockchainDevelopersTransaction

  override val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def serializer: Serializer[BlockchainDevelopersTransaction] = BCTransactionSerializer

  override def json: Json = this.asJson
}

object BCTransactionSerializer extends Serializer[BlockchainDevelopersTransaction] {
  import org.msgpack.core.MessagePack

  override def toBytes(obj: BlockchainDevelopersTransaction): Array[Byte] = {
    val packer = MessagePack.newDefaultBufferPacker()
    packer.packArrayHeader(obj.inputs.size)
    for {
      input <- obj.inputs
    } yield {
      packer.packBinaryHeader(input.length)
      packer.writePayload(input)
    }
    packer.packArrayHeader(obj.outputs.size)
    for {
      output <- obj.outputs
    } yield {
      packer.packBinaryHeader(output._1.hash.length)
      packer.writePayload(output._1.hash)
      packer.packLong(output._2)
    }
    packer.packArrayHeader(obj.signatures.size)
    for {
      signature <- obj.signatures
    } yield {
      packer.packBinaryHeader(signature.preimage.length)
      packer.writePayload(signature.preimage)
    }
    packer.toByteArray
  }

  override def parseBytes(bytes: Array[Byte]): Try[BlockchainDevelopersTransaction] = Try {
    val unpacker = MessagePack.newDefaultUnpacker(bytes)
    val numInputs = unpacker.unpackArrayHeader()
    val inputs = for {
      i <- Range(0, numInputs)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      OutputId @@ unpacker.readPayload(binaryLen)
    }
    val numOutputs = unpacker.unpackArrayHeader()
    val outputs = for {
      i <- Range(0, numOutputs)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      (
        Sha256PreimageProposition(Digest32 @@ unpacker.readPayload(binaryLen)),
        Value @@ unpacker.unpackLong()
      )
    }
    val numTransactions = unpacker.unpackArrayHeader()
    val transactions = for {
      i <- Range(0, numTransactions)
    } yield {
      val binaryLen = unpacker.unpackBinaryHeader()
      Sha256PreimageProof(Digest32Preimage @@ unpacker.readPayload(binaryLen))
    }
    BlockchainDevelopersTransaction(inputs, outputs, transactions)
  }
}