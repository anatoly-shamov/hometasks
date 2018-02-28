package transaction

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.hash.Sha256

class Sha256PreimageProofTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators {

  property("proof should be correctly validated") {
    forAll(preimageProofGen, nonEmptyBytesGen) { (proof, msg) =>
      val publicImage = Sha256PreimageProposition(Sha256(proof.preimage))
      proof.isValid(publicImage, msg) shouldBe true
    }
  }

  property("proof should be serialized and deserialized successfully") {
    forAll(preimageProofGen) { proof =>
      val bytes = proof.serializer.toBytes(proof)
      val proof2 = proof.serializer.parseBytes(bytes)
      proof equals proof2
    }
  }
}
