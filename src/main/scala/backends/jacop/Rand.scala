package backends.jacop

import org.jacop.constraints.XplusYeqZ
import org.jacop.core.IntDomain
import org.jacop.scala.{Model2, getModel}

class Rand(name: String, min: Int, max: Int)(implicit val model: Model2) extends backends.jacop.IntVar(name, min, max)(model) with crv.Rand {

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
