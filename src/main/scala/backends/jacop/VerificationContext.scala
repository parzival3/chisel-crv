package backends.jacop

trait VerificationContext extends crv.VerificationContext {

  /**
    * Converts integer to IntVar.
    *
    * @param i integer to be converted.
    */
  implicit def BigIntToRand(i: BigInt)(implicit model: Model): Rand = {
    require(i < Int.MaxValue)
    new Rand(i.toInt, i.toInt)
  }
  implicit def RandToInt(r:   Rand): Int = r.value()
  implicit def IntToBigInt(i: Int):  BigInt = BigInt(i)
}
