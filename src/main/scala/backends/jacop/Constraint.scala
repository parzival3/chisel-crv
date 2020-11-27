package backends.jacop

import java.util

import org.jacop.core.{Store, Var}

class Constraint(private val constraint: org.jacop.constraints.PrimitiveConstraint)(implicit randObj: RandObj) extends crv.Constraint {

  def disable(): Unit = {
    randObj.model.constr -= constraint
    constraint.removeConstraint()
    randObj.initialized = false
    randObj.resetDomains()
  }

  def enable(): Unit = {
    randObj.model.constr += constraint
    randObj.model.addChanged(constraint)
    randObj.resetDomains()
  }

  /**
   * It specifies the events which must occur for notConsistency()
   * method being executed.
   */
  var notConsistencyPruningEvents: util.Hashtable[Var, Integer] = constraint.notConsistencyPruningEvents

  /**
   * It retrieves the pruning event which causes reevaluation of the
   * constraint notConsistency() function.
   *
   * @param var for which pruning event is retrieved
   * @return the int denoting the pruning event associated with given variable.
   */
  def getNotConsistencyPruningEvent(`var`: Var): Int = constraint.getNotConsistencyPruningEvent(`var`)

  def impose(store: Store): Unit = constraint.impose(store)

  /**
   * It retrieves the pruning event for which any composed constraint which
   * uses this constraint should be evaluated. This events are the ones which
   * can change satisfied status?
   *
   * @param var  for which pruning event is retrieved
   * @param mode decides if pruning event for consistency or nonconsistency is required.
   * @return pruning event associated with the given variable for a given consistency mode.
   */
  def getNestedPruningEvent(`var`: Var, mode: Boolean): Int = getNestedPruningEvent(`var`, mode)

  /**
   * It makes pruning in such a way that constraint is notConsistent. It
   * removes values which always belong to a solution.
   *
   * @param store the constraint store in which context the notConsistency technique is evaluated.
   */
  def notConsistency(store: Store): Unit = constraint.notConsistency(store)

  /**
   * It checks if constraint would be always not satisfied.
   *
   * @return true if constraint must be notSatisfied, false otherwise.
   */
  def notSatisfied: Boolean = constraint.notSatisfied()

  /**
   * It allows to specify customized events required to trigger execution
   * of notConsitency() method.
   *
   * @param var          variable for which customized event is setup.
   * @param pruningEvent the type of the event being setup.
   */
  def setNotConsistencyPruningEvent(`var`: Var, pruningEvent: Int): Unit = constraint.setNotConsistencyPruningEvent(`var`, pruningEvent)

  def include(store: Store): Unit = constraint.include(store)
}
