package transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators

class BDTransactionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with CoreGenerators
{
  property("tx should be serialized to byte array and deserialized from it") {
    val tx = BlockchainDevelopersTransaction(
      Array[OutputId](OutputId @@ Array[Byte](0, 1, 2, 3)),
      Array[(Sha256PreimageProposition, Value)]((Sha256PreimageProposition(Digest32 @@ Array[Byte](4, 5, 6, 7)), Value @@ 42L)),
      Array[Sha256PreimageProof](Sha256PreimageProof(Digest32Preimage @@ Array[Byte](8, 9, 0, 1)))
    )
    val buf = tx.serializer.toBytes(tx)
    val tx2 = tx.serializer.parseBytes(buf).get
    tx equals tx2
  }

  property("tx should be serialized in json") {
    val tx = BlockchainDevelopersTransaction(
      Array[OutputId](OutputId @@ Array[Byte](0, 1, 2, 3)),
      Array[(Sha256PreimageProposition, Value)]((Sha256PreimageProposition(Digest32 @@ Array[Byte](0, 1, 2, 3)), Value @@ 42L)),
      Array[Sha256PreimageProof](Sha256PreimageProof(Digest32Preimage @@ Array[Byte](0, 1, 2, 3)))
    )
    val json = tx.json
  }
}