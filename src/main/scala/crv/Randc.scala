package crv

trait Randc {
  def value(): BigInt
  def next():  Unit
  def setVar(that: BigInt): Unit

  override def toString: String = value().toString
}
