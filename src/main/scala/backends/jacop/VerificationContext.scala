package backends.jacop

trait VerificationContext extends crv.VerificationContext {

  /**
    * Converts integer to IntVar.
    *
    * @param i integer to be converted.
    */
  implicit def IntToRand(i: Int)(implicit obj: RandObj): Rand = new Rand(i, i)
  implicit def RandToInt(r: Rand): Int = r.value()
}
