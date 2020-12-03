package crv

trait Constraint {
  type U
  def enable():      Unit
  def disable():     Unit
  def getConstraint: U
}
