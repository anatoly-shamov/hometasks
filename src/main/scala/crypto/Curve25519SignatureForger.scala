package crypto

import scorex.crypto.signatures.Signature

object Curve25519SignatureForger {

  def forgeSignature(signature: Signature): Signature = {
    // l parameter for Ed25519-SHA-512
    // see https://ed25519.cr.yp.to/ed25519-20110926.pdf, https://eprint.iacr.org/2015/677.pdf
    val l = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")

    Signature(
      signature.take(32) ++ //R
        (BigInt(signature.takeRight(32).reverse) + l).toByteArray.reverse //S + l
    )
  }

}
