package crv

trait Constraint {
  def enable():  Unit
  def disable(): Unit
}
