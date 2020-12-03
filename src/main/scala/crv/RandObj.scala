package crv

trait RandObj {

  /**
    * Randomize the current object
    * @return Boolean the result of the current randomization
    */
  def randomize: Boolean

  def preRandomize(): Unit = {}

  def postRandomize(): Unit = {}

  def ifThen(ifC: Constraint)(thenC: Constraint): Constraint

  def ifThenElse(ifC: Constraint)(thenC: Constraint)(elseC: Constraint): Constraint
}
