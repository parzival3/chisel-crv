package backends.jacop

import scala.util.Random

class Randc(val min: BigInt, val max: BigInt)(implicit model: Model) extends crv.Randc {
  model.randcVars += this

  private val rand = new Random(model.seed)
  private var currentValue: BigInt = (math.abs(rand.nextInt) % (max - min)) + min

  def value(): BigInt = currentValue
  def next():  Unit = currentValue = if (currentValue == max) min else currentValue + 1

  override def setVar(that: BigInt): Unit = {
    currentValue = that
  }

}
