package transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Sha256

class Sha256PreimagePropositionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators {

  property("proposition should be serialized and deserialized successfully") {
    forAll(propositionGen) { prop =>
      val bytes = prop.serializer.toBytes(prop)
      val prop2 = prop.serializer.parseBytes(bytes)
      prop equals prop2
    }
  }
}
