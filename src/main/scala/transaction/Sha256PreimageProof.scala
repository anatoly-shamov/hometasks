package transaction

import scorex.core.serialization.Serializer
import scorex.core.transaction.proof.ProofOfKnowledge
import scorex.crypto.hash.Sha256

import scala.util.Try

case class Sha256PreimageProof(preimage: Digest32Preimage) extends ProofOfKnowledge[Sha256Preimage, Sha256PreimageProposition]{
  override type M = Sha256PreimageProof

  override def isValid(proposition: Sha256PreimageProposition, message: Array[Byte]): Boolean =
    proposition.hash sameElements Sha256(preimage)

  override def serializer: Serializer[Sha256PreimageProof] = Sha256PreimageProofSerializer

  override def equals(obj: scala.Any): Boolean = obj match {
    case proof: Sha256PreimageProof => preimage sameElements proof.preimage
    case _ => false
  }
}

object Sha256PreimageProofSerializer extends Serializer[Sha256PreimageProof] {
  override def toBytes(obj: Sha256PreimageProof): Array[Byte] = obj.preimage

  override def parseBytes(bytes: Array[Byte]): Try[Sha256PreimageProof] =
    Try(Sha256PreimageProof(Digest32Preimage @@ bytes))
}

