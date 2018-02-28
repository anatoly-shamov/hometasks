package transaction

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class BDTransactionTest extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with Generators
{
  property("tx should be serialized to byte array and deserialized from it") {
    forAll (transactionGen) {tx =>
      val buf = tx.serializer.toBytes(tx)
      val tx2 = tx.serializer.parseBytes(buf).get
      tx equals tx2
    }
  }

  property("tx should be serialized in json") {
    forAll (transactionGen) {tx =>
      val json = tx.json
    }
  }
}