package crv

// TODO: we can avoid specifying the name by using reflection
trait Rand[T] {
  /**
    * Defines the add constraint between two Rand variables
    * @param that a second parameter for the addition constraint
    * @return Rand variable being the result of the addition constraint.
    */
  type U <: T
  def +(that: U): U

  def -(that: U): U

  def *(that: U): U

  def div(that: U): U

  def mod(that: U): U

  def ^(that: U): U

  def #\=(that: U): Constraint

  def #<(that: U): Constraint

  def #<=(that: U): Constraint

  def #>(that: U): Constraint

  def #>=(that: U): Constraint
}
