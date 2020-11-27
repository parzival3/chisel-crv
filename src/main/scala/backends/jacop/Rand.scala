package backends.jacop

import org.jacop.constraints.{XdivYeqZ, XeqY, XexpYeqZ, XgtY, XgteqY, XltY, XlteqY, XmodYeqZ, XmulYeqZ, XneqY, XplusYeqC, XplusYeqZ}
import org.jacop.core.IntDomain
import org.jacop.scala.{IntSet, getModel}

class Rand(name: String, min: Int, max: Int)(implicit val obj: RandObj) extends org.jacop.scala.IntVar(name, min, max) with crv.Rand {

  /**
   * Defines an anonymous finite domain integer variable.
   *
   * @constructor Creates a new finite domain integer variable.
   * @param min minimal value of variable's domain.
   * @param max maximal value of variable's domain.
   */
  def this(min: Int, max: Int)(implicit obj: RandObj) = {
    this("_$" + getModel.n, min, max)(obj)
    getModel.n += 1
  }

  /**
   * Defines an anonymous finite domain integer variable.
   *
   * @constructor Creates a new finite domain integer variable with minimal and maximal
   *              values in the domain defined by org.jacop.
   * @param name variable's identifier.
   */
  def this(name: String)(implicit obj: RandObj) = {
    this(name, org.jacop.core.IntDomain.MinInt, org.jacop.core.IntDomain.MaxInt)(obj)
    getModel.n += 1
  }

  /**
   * Defines an anonymous finite domain integer variable.
   *
   * @constructor Creates a new finite domain integer variable with minimal and maximal
   *              values in the domain defined by org.jacop.
   */
  def this()(implicit obj: RandObj) = {
    this(org.jacop.core.IntDomain.MinInt, org.jacop.core.IntDomain.MaxInt)(obj)
    getModel.n += 1
  }

  /**
   * Defines a finite domain integer variable.
   *
   * @constructor Create a new finite domain integer variable with the domain defined by IntSet.
   * @param dom variable's domain defined as a set of integers (IntSet).
   */
  def this(dom: IntSet)(implicit obj: RandObj) = {
    this()(obj)
    this.dom.intersectAdapt(dom)
    getModel.n += 1
  }

  /**
   * Defines a finite domain integer variable.
   *
   * @constructor Create a new finite domain integer variable with the domain
   *              defined by IntSet.
   * @param name variable's identifier.
   * @param dom  variable's domain defined as a set of integers (IntSet).
   */
  def this(name: String, dom: IntSet)(implicit obj: RandObj) = {
    this(name)(obj)
    this.dom.intersectAdapt(dom)
    getModel.n += 1
  }

  /**
   * Defines the add constraint between two Rand variables
   *
   * @param that a second parameter for the addition constraint
   * @return Rand variable being the result of the addition constraint.
   */
   def +(that: backends.jacop.Rand): crv.Rand = {
     val result = new Rand(IntDomain.addInt(this.min(), that.min()), IntDomain.addInt(this.max(), that.max()))
     val c = new XplusYeqZ(this, that, result)
     getModel.constr += c
     result
  }

  /**
   * Defines subtract constraint between two IntVar.
   *
   * @param that a second parameter for the subtraction constraint.
   * @return IntVar variable being the result of the subtraction constraint.
   */
  def -(that: backends.jacop.Rand) : crv.Rand = {
    val result = new Rand(IntDomain.subtractInt(this.min(), that.max()), IntDomain.subtractInt(this.max(), that.min()))
    val c = new XplusYeqZ(result, that, this)
    getModel.constr += c
    result
  }

  /**
   * Defines equation constraint between two IntVar.
   *
   * @param that a second parameter for equation constraint.
   * @return the defined constraint.
   */
  def #=(that: crv.Rand): Constraint = {
    that match {
      case v: backends.jacop.Rand => {
        val c = new XeqY(this, v.asInstanceOf[org.jacop.core.IntVar])
        getModel.constr += c
        new Constraint(c)
      }
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }

  /**
   * Defines the add constraint between two Rand variables
   *
   * @param that a second parameter for the addition constraint
   * @return Rand variable being the result of the addition constraint.
   */
  override def +(that: crv.Rand): crv.Rand = {
    that match {
      case v: backends.jacop.Rand => this.+(v)
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }

  /**
   * Defines subtract constraint between two IntVar.
   *
   * @param that a second parameter for the subtraction constraint.
   * @return IntVar variable being the result of the subtraction constraint.
   */
  def -(that: crv.Rand) : crv.Rand = {
    that match {
      case v: backends.jacop.Rand => this.-(v)
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }

  /**
   * Defines multiplication constraint between two IntVar.
   *
   * @param that a second parameter for the multiplication constraint.
   * @return IntVar variable being the result of the multiplication constraint.
   */
  def *(that: backends.jacop.Rand): crv.Rand = {
    val bounds = IntDomain.mulBounds(this.min(), this.max(), that.min(), that.max())
    val result = new Rand(bounds.min(), bounds.max())
    val c = new XmulYeqZ(this, that, result)
    getModel.constr += c
    result
  }

  /**
   * Defines multiplication constraint between two IntVar.
   *
   * @param that a second parameter for the multiplication constraint.
   * @return IntVar variable being the result of the multiplication constraint.
   */
  def *(that: crv.Rand): crv.Rand = {
    that match {
      case v: backends.jacop.Rand => this.*(v)
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }

  /**
   * Defines integer division constraint between two IntVar.
   *
   * @param that a second parameter for the integer division constraint.
   * @return IntVar variable being the result of the integer division constraint.
   */
  def div(that: backends.jacop.Rand): crv.Rand = {
    val bounds = IntDomain.divBounds(this.min(), this.max(), that.min(), that.max())
    val result = new Rand(bounds.min(), bounds.max())
    val c = new XdivYeqZ(this, that, result)
    getModel.constr += c
    result
  }

  /**
   * Defines integer division constraint between two IntVar.
   *
   * @param that a second parameter for the integer division constraint.
   * @return IntVar variable being the result of the integer division constraint.
   */
  def div(that: crv.Rand): crv.Rand = {
    that match {
      case v: backends.jacop.Rand => this.div(v)
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }

  /**
   * Defines constraint for integer reminder from division between two IntVar.
   *
   * @param that a second parameter for integer reminder from division constraint.
   * @return IntVar variable being the result of the integer reminder from division constraint.
   */
  def mod(that: backends.jacop.Rand): crv.Rand = {
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

    val result = new Rand(reminderMin, reminderMax)
    val c = new XmodYeqZ(this, that, result)
    getModel.constr += c
    result
  }

  /**
   * Defines constraint for integer reminder from division between two IntVar.
   *
   * @param that a second parameter for integer reminder from division constraint.
   * @return IntVar variable being the result of the integer reminder from division constraint.
   */
  def mod(that: crv.Rand): crv.Rand = {
    that match {
      case v: backends.jacop.Rand => this.mod(v)
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }

  /**
   * Defines exponentiation constraint between two IntVar.
   *
   * @param that exponent for the exponentiation constraint.
   * @return IntVar variable being the result of the exponentiation constraint.
   */
  def ^(that: backends.jacop.Rand): crv.Rand = {
    val result = new Rand()
    val c = new XexpYeqZ(this, that, result)
    getModel.constr += c
    result
  }

  /**
   * Defines exponentiation constraint between two IntVar.
   *
   * @param that exponent for the exponentiation constraint.
   * @return IntVar variable being the result of the exponentiation constraint.
   */
  def ^(that: crv.Rand): crv.Rand = {
    that match {
      case v: backends.jacop.Rand => this.^(v)
    }
  }

  /**
   * Defines unary "-" constraint for IntVar.
   *
   * @return the defined constraint.
   */
  override def unary_- : Rand = {
    val result = new Rand(-this.max(), -this.min())
    val c = new XplusYeqC(this, result, 0)
    getModel.constr += c
    result
  }

  /**
   * Defines inequality constraint between two IntVar.
   *
   * @param that a second parameter for inequality constraint.
   * @return the defined constraint.
   */
  def #\=(that: backends.jacop.Rand): Constraint = {
    val c = new XneqY(this, that)
    getModel.constr += c
    new Constraint(c)
  }

  /**
   * Defines "less than" constraint between two IntVar.
   *
   * @param that a second parameter for "less than" constraint.
   * @return the defined constraint.
   */
  def #<(that: backends.jacop.Rand): Constraint = {
    val c = new XltY(this, that)
    getModel.constr += c
    new Constraint(c)
  }

  /**
   * Defines "less than or equal" constraint between two IntVar.
   *
   * @param that a second parameter for "less than or equal" constraint.
   * @return the defined constraint.
   */
  def #<=(that: backends.jacop.Rand): Constraint = {
    val c = new XlteqY(this, that)
    getModel.constr += c
    new Constraint(c)
  }

  /**
   * Defines "greater than" constraint between two IntVar.
   *
   * @param that a second parameter for "greater than" constraint.
   * @return the defined constraint.
   */
  def #>(that: backends.jacop.Rand): Constraint = {
    val c = new XgtY(this, that)
    getModel.constr += c
    new Constraint(c)
  }

  /**
   * Defines "greater than or equal" constraint between two IntVar.
   *
   * @param that a second parameter for "greater than or equal" constraint.
   * @return the defined constraint.
   */
  def #>=(that: backends.jacop.Rand): Constraint = {
    val c = new XgteqY(this, that)
    getModel.constr += c
    new Constraint(c)
  }
}
