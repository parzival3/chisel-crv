package backends.jacop

import scala.util.Random

class Randc(val min: Int, val max: Int, val seed: Int = 0)(implicit model: Model) extends crv.Randc {

  model.randcVars += this

  private val rand = if (seed != 0) new Random(seed) else new Random
  private var currentValue: Int = (math.abs(rand.nextInt) % (max - min)) + min

  def value(): Int = currentValue
  def next():  Unit = currentValue = if (currentValue == max) min else currentValue + 1

}
