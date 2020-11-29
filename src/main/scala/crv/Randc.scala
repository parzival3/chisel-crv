package crv

trait Randc {
  def value(): Int
  def next():  Unit

  override def toString: String = value().toString
}
