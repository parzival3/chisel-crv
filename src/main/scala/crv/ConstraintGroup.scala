package crv

trait ConstraintGroup {
  val constraints: List[Constraint]
  def enable():  Unit = constraints.foreach(_.enable())
  def disable(): Unit = constraints.foreach(_.disable())
}
