import io.circe.{Encoder, Json}
import io.circe.syntax.EncoderOps
import scorex.core.{ModifierId, ModifierTypeId}
import supertagged.TaggedType
import supertagged.untag

package object transaction {
  object Digest32Preimage extends TaggedType[Array[Byte]]

  type Digest32Preimage = Digest32Preimage.Type

  object OutputId extends TaggedType[Array[Byte]]

  type OutputId = OutputId.Type

  object Value extends TaggedType[Long]

  type Value = Value.Type

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
}
