package backends.jacop

import org.jacop.constraints.{PrimitiveConstraint, XdivYeqZ, XeqC, XeqY, XexpYeqZ, XgtC, XgtY, XgteqC, XgteqY, XltC, XltY, XlteqC, XlteqY, XmodYeqZ, XmulCeqZ, XmulYeqZ, XneqC, XneqY, XplusCeqZ, XplusYeqC, XplusYeqZ}
import org.jacop.core.IntDomain
import org.jacop.scala.{IntSet, Model2, SetVar, jacop}
import org.jacop.set.constraints.{EinA, XinA}


/**
 * Defines a finite domain integer variable and its primitive constraints.
 *
 * @constructor Creates a new finite domain integer variable.
 * @param name variable identifier.
 * @param min  minimal value of variable's domain.
 * @param max  maximal value of variable's domain.
 */
class IntVar(name: String, min: Int, max: Int)(implicit model: Model2) extends org.jacop.core.IntVar(model, name, min, max) with jacop {

  /**
   * Defines an anonymous finite domain integer variable.
   *
   * @constructor Creates a new finite domain integer variable.
   * @param min minimal value of variable's domain.
   * @param max maximal value of variable's domain.
   */
  def this(min: Int, max: Int)(implicit model: Model2) = {
    this("_$" + model.n, min, max)
    model.n += 1
  }

  /**
   * Defines an anonymous finite domain integer variable.
   *
   * @constructor Creates a new finite domain integer variable with minimal and maximal
   *              values in the domain defined by org.jacop.
   * @param name variable's identifier.
   */
  def this(name: String)(implicit model: Model2) = {
    this(name, org.jacop.core.IntDomain.MinInt, org.jacop.core.IntDomain.MaxInt)
    model.n += 1
  }

  /**
   * Defines an anonymous finite domain integer variable.
   *
   * @constructor Creates a new finite domain integer variable with minimal and maximal
   *              values in the domain defined by org.jacop.
   */
  def this()(implicit model: Model2) = {
    this(org.jacop.core.IntDomain.MinInt, org.jacop.core.IntDomain.MaxInt)
    model.n += 1
  }

  /**
   * Defines a finite domain integer variable.
   *
   * @constructor Create a new finite domain integer variable with the domain defined by IntSet.
   * @param dom variable's domain defined as a set of integers (IntSet).
   */
  def this(dom: IntSet)(implicit model: Model2) = {
    this()
    this.dom.intersectAdapt(dom)
    model.n += 1
  }

  /**
   * Defines a finite domain integer variable.
   *
   * @constructor Create a new finite domain integer variable with the domain
   *              defined by IntSet.
   * @param name variable's identifier.
   * @param dom  variable's domain defined as a set of integers (IntSet).
   */
  def this(name: String, dom: IntSet)(implicit model: Model2) = {
    this(name)
    this.dom.intersectAdapt(dom)
    model.n += 1
  }

  /**
   * Defines add constraint between two IntVar.
   *
   * @param that a second parameter for the addition constraint.
   * @return IntVar variable being the result of the addition constraint.
   */
  def +(that: org.jacop.core.IntVar) = {
    val result = new IntVar(IntDomain.addInt(this.min(), that.min()), IntDomain.addInt(this.max(), that.max()))
    val c = new XplusYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines add constraint between IntVar and an integer value.
   *
   * @param that a second integer parameter for the addition constraint.
   * @return IntVar variable being the result of the addition constraint.
   */
  def +(that: Int) = {
    val result = new IntVar(IntDomain.addInt(this.min(), that), IntDomain.addInt(this.max(), that))
    val c = new XplusCeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines subtract constraint between two IntVar.
   *
   * @param that a second parameter for the subtraction constraint.
   * @return IntVar variable being the result of the subtraction constraint.
   */
  def -(that: org.jacop.core.IntVar) = {
    val result = new IntVar(IntDomain.subtractInt(this.min(), that.max()), IntDomain.subtractInt(this.max(), that.min()))
    val c = new XplusYeqZ(result, that, this)
    model.constr += c
    result
  }

  /**
   * Defines subtract constraint between IntVar and an integer value.
   *
   * @param that a second integer parameter for the subtraction constraint.
   * @return IntVar variable being the result of the subtraction constraint.
   */
  def -(that: Int) = {
    val result = new IntVar(IntDomain.subtractInt(this.min(), that), IntDomain.subtractInt(this.max(), that))
    val c = new XplusCeqZ(result, that, this)
    model.constr += c
    result
  }

  /**
   * Defines multiplication constraint between two IntVar.
   *
   * @param that a second parameter for the multiplication constraint.
   * @return IntVar variable being the result of the multiplication constraint.
   */
  def *(that: org.jacop.core.IntVar) = {
    val bounds = IntDomain.mulBounds(this.min(), this.max(), that.min(), that.max())
    val result = new IntVar(bounds.min(), bounds.max())
    val c = new XmulYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines multiplication constraint between IntVar and an integer value.
   *
   * @param that a second integer parameter for the multiplication constraint.
   * @return IntVar variable being the result of the multiplication constraint.
   */
  def *(that: Int) = {
    val bounds = IntDomain.mulBounds(this.min(), this.max(), that, that)
    val result = new IntVar(bounds.min(), bounds.max())
    val c = new XmulCeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines integer division constraint between two IntVar.
   *
   * @param that a second parameter for the integer division constraint.
   * @return IntVar variable being the result of the integer division constraint.
   */
  def div(that: org.jacop.core.IntVar) = {
    val bounds = IntDomain.divBounds(this.min(), this.max(), that.min(), that.max())
    val result = new IntVar(bounds.min(), bounds.max())
    val c = new XdivYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines constraint for integer reminder from division between two IntVar.
   *
   * @param that a second parameter for integer reminder from division constraint.
   * @return IntVar variable being the result of the integer reminder from division constraint.
   */
  def mod(that: org.jacop.core.IntVar) = {
    var reminderMin: Int = 0;
    var reminderMax: Int = 0;

    if (this.min() >= 0) {
      reminderMin = 0
      reminderMax = Math.max(Math.abs(that.min()), Math.abs(that.max())) - 1
    }
    else if (this.max() < 0) {
      reminderMax = 0
      reminderMin = -Math.max(Math.abs(that.min()), Math.abs(that.max())) + 1
    }
    else {
      reminderMin = Math.min(Math.min(that.min(), -that.min()), Math.min(that.max(), -that.max())) + 1
      reminderMax = Math.max(Math.max(that.min(), -that.min()), Math.max(that.max(), -that.max())) - 1
    }

    val result = new IntVar(reminderMin, reminderMax)
    val c = new XmodYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines exponentiation constraint between two IntVar.
   *
   * @param that exponent for the exponentiation constraint.
   * @return IntVar variable being the result of the exponentiation constraint.
   */
  def ^(that: org.jacop.core.IntVar) = {
    val result = new IntVar()
    val c = new XexpYeqZ(this, that, result)
    model.constr += c
    result
  }

  /**
   * Defines unary "-" constraint for IntVar.
   *
   * @return the defined constraint.
   */
  def unary_- = {
    val result = new IntVar(-this.max(), -this.min())
    val c = new XplusYeqC(this, result, 0)
    model.constr += c
    result
  }

  /**
   * Defines equation constraint between two IntVar.
   *
   * @param that a second parameter for equation constraint.
   * @return the defined constraint.
   */
  @deprecated("use #= instead", "1.0")
  def ==(that: org.jacop.core.IntVar) = {
    val c = new XeqY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines equation constraint between two IntVar.
   *
   * @param that a second parameter for equation constraint.
   * @return the defined constraint.
   */
  def #=(that: org.jacop.core.IntVar) = {
    val c = new XeqY(this, that)
    model.constr += c
    c
  }


  /**
   * Defines equation constraint between an IntVar and FloatVar.
   *
   * @param that a second parameter for equation constraint.
   * @return the defined constraint.
   */
  def #=(that: org.jacop.floats.core.FloatVar) = {
    val c = new org.jacop.floats.constraints.XeqP(this, that)
    model.constr += c
    c
  }

  /**
   * Defines equation constraint between IntVar and a integer constant.
   *
   * @param that a second parameter for equation constraint.
   * @return the defined constraint.
   */
  @deprecated("use #= instead", "1.0")
  def ==(that: Int) = {
    val c = new XeqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines equation constraint between IntVar and a integer constant.
   *
   * @param that a second parameter for equation constraint.
   * @return the defined constraint.
   */
  def #=(that: Int) = {
    val c = new XeqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines inequality constraint between two IntVar.
   *
   * @param that a second parameter for inequality constraint.
   * @return the defined constraint.
   */
  @deprecated("use #\\= instead", "1.0")
  def !=(that: org.jacop.core.IntVar) = {
    val c = new XneqY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines inequality constraint between two IntVar.
   *
   * @param that a second parameter for inequality constraint.
   * @return the defined constraint.
   */
  def #\=(that: org.jacop.core.IntVar) = {
    val c = new XneqY(this, that)
    model.constr += c
    c
  }


  /**
   * Defines inequality constraint between IntVar and integer constant.
   *
   * @param that a second parameter for inequality constraint.
   * @return the defined constraint.
   */
  @deprecated("use #\\= instead", "1.0")
  def !=(that: Int) = {
    val c = new XneqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines inequality constraint between IntVar and integer constant.
   *
   * @param that a second parameter for inequality constraint.
   * @return the defined constraint.
   */
  def #\=(that: Int) = {
    val c = new XneqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than" constraint between two IntVar.
   *
   * @param that a second parameter for "less than" constraint.
   * @return the defined constraint.
   */
  @deprecated("use #< instead", "1.0")
  def <(that: org.jacop.core.IntVar) = {
    val c = new XltY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than" constraint between two IntVar.
   *
   * @param that a second parameter for "less than" constraint.
   * @return the defined constraint.
   */
  def #<(that: org.jacop.core.IntVar) = {
    val c = new XltY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "less than" constraint.
   * @return the equation constraint.
   */
  @deprecated("use #< instead", "1.0")
  def <(that: Int) = {
    val c = new XltC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "less than" constraint.
   * @return the equation constraint.
   */
  def #<(that: Int) = {
    val c = new XltC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than or equal" constraint between two IntVar.
   *
   * @param that a second parameter for "less than or equal" constraint.
   * @return the defined constraint.
   */
  @deprecated("use #<= instead", "1.0")
  def <=(that: org.jacop.core.IntVar) = {
    val c = new XlteqY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than or equal" constraint between two IntVar.
   *
   * @param that a second parameter for "less than or equal" constraint.
   * @return the defined constraint.
   */
  def #<=(that: org.jacop.core.IntVar) = {
    val c = new XlteqY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than or equal" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "less than or equal" constraint.
   * @return the equation constraint.
   */
  @deprecated("use #<= instead", "1.0")
  def <=(that: Int) = {
    val c = new XlteqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "less than or equal" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "less than or equal" constraint.
   * @return the equation constraint.
   */
  def #<=(that: Int) = {
    val c = new XlteqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than" constraint between two IntVar.
   *
   * @param that a second parameter for "greater than" constraint.
   * @return the defined constraint.
   */
  @deprecated("use #> instead", "1.0")
  def >(that: org.jacop.core.IntVar) = {
    val c = new XgtY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than" constraint between two IntVar.
   *
   * @param that a second parameter for "greater than" constraint.
   * @return the defined constraint.
   */
  def #>(that: org.jacop.core.IntVar) = {
    val c = new XgtY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "greater than" constraint.
   * @return the equation constraint.
   */
  @deprecated("use #> instead", "1.0")
  def >(that: Int) = {
    val c = new XgtC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "greater than" constraint.
   * @return the equation constraint.
   */
  def #>(that: Int) = {
    val c = new XgtC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than or equal" constraint between two IntVar.
   *
   * @param that a second parameter for "greater than or equal" constraint.
   * @return the defined constraint.
   */
  @deprecated("use #>= instead", "1.0")
  def >=(that: org.jacop.core.IntVar) = {
    val c = new XgteqY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than or equal" constraint between two IntVar.
   *
   * @param that a second parameter for "greater than or equal" constraint.
   * @return the defined constraint.
   */
  def #>=(that: org.jacop.core.IntVar) = {
    val c = new XgteqY(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than or equal" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "greater than or equal" constraint.
   * @return the equation constraint.
   */
  @deprecated("use #>= instead", "1.0")
  def >=(that: Int) = {
    val c = new XgteqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines "greater than or equal" constraint between IntVar and integer constant.
   *
   * @param that a second parameter for "greater than or equal" constraint.
   * @return the equation constraint.
   */
  def #>=(that: Int) = {
    val c = new XgteqC(this, that)
    model.constr += c
    c
  }

  /**
   * Defines constraint on inclusion of a IntVar variable value in a set.
   *
   * @param that set that this variable's value must be included.
   * @return the equation constraint.
   */
  def in(that: SetVar): PrimitiveConstraint = {
    if (min == max) {
      val c = new EinA(min, that)
      model.constr += c
      c
    }
    else {
      val c = new XinA(this, that)
      model.constr += c
      c
    }
  }
}
