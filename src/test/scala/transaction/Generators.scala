package transaction

import org.scalacheck.Gen
import scorex.crypto.hash.Digest32
import scorex.testkit.generators.CoreGenerators

trait Generators extends CoreGenerators {
  val inputGen: Gen[OutputId] =
    genBytesList(32).map(bytes => OutputId @@ bytes)

  val preimagePropositionGen: Gen[Sha256PreimageProposition] =
    nonEmptyBytesGen.map(bytes => Sha256PreimageProposition(Digest32 @@ bytes))

  val valueGen: Gen[Value] =
    positiveLongGen.map(long => Value @@ long)

  val outputGen: Gen[(Sha256PreimageProposition, Value)] = for {
    proposition <- preimagePropositionGen
    value <- valueGen
  } yield (proposition, value)

  val preimageProofGen: Gen[Sha256PreimageProof] =
    nonEmptyBytesGen.map(bytes => Sha256PreimageProof(Digest32Preimage @@ bytes))

  val transactionGen: Gen[BlockchainDevelopersTransaction] = for {
    inputs <- Gen.nonEmptyListOf(inputGen)
    outputs <- Gen.nonEmptyListOf(outputGen)
    signatures <- Gen.listOfN(inputs.length, preimageProofGen)
  } yield BlockchainDevelopersTransaction(inputs.toIndexedSeq, outputs.toIndexedSeq, signatures.toIndexedSeq)
}
