package crv

abstract class Constraint {
  def enable():  Unit
  def disable(): Unit
}
