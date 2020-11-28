package crv

abstract class Constraint {
  abstract def enable(): Unit
  abstract def disable(): Unit
}
