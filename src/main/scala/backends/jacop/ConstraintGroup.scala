package backends.jacop

class ConstraintGroup(group: Constraint*) extends crv.ConstraintGroup {
  override val constraints: List[Constraint] = group.toList
}
