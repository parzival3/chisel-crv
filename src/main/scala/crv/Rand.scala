package crv

// TODO: we can avoid specifying the name by using reflection
trait Rand {

  /**
    * Defines the add constraint between two Rand variables
    * @param that a second parameter for the addition constraint
    * @return Rand variable being the result of the addition constraint.
    */
  def +(that: Rand): Rand

}
