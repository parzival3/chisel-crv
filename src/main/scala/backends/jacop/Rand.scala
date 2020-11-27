package backends.jacop

import org.jacop.constraints.{XeqY, XplusYeqZ}
import org.jacop.core.{IntDomain, IntVar}
import org.jacop.scala.{getModel}

class Rand(name: String, min: Int, max: Int)(implicit val obj: RandObj) extends org.jacop.scala.IntVar(name, min, max) with crv.Rand {

  /**
   * Defines the add constraint between two Rand variables
   *
   * @param that a second parameter for the addition constraint
   * @return Rand variable being the result of the addition constraint.
   */
   def +(that: backends.jacop.Rand): crv.Rand = {
     val result = new Rand("_$" + getModel.n, IntDomain.addInt(this.min(), that.min()), IntDomain.addInt(this.max(), that.max()))
     val c = new XplusYeqZ(this, that, result)
     getModel.constr += c
     getModel.n += 1
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
        val c = new XeqY(this, v.asInstanceOf[IntVar])
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
      case v: backends.jacop.Rand => this.+(v.asInstanceOf[backends.jacop.Rand])
      case _ => throw new Exception("Can't mix in two different backends")
    }
  }
}
