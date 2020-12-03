package backends.jacop

import scala.util.Random

class Model(val seed: Int = new Random().nextInt()) extends org.jacop.scala.Model {
  import scala.collection.mutable.ListBuffer
  val randcVars = new ListBuffer[Randc]
}
