package org.jacop.scala

class Model2 extends Model {
  override def imposeAllConstraints() {
    this.constr.foreach(e => this.impose(e))
    // this.constr.clear()
  }
}
