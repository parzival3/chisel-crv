package crv

class ConstraintGroup(group: Constraint*) {
  val constraints: List[Constraint] = group.toList
  def enable():    Unit = constraints.foreach(_.enable())
  def disable():   Unit = constraints.foreach(_.disable())
}
