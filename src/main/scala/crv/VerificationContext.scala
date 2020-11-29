package crv

trait VerificationContext {
  implicit def randcToIn(v: Randc): Int = v.value
}
