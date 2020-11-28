package backends.jacop
import org.jacop.scala.Model

class RandArr(implicit model: Model) extends RandObj {
  var size: Rand = new Rand("size")
  var sum:  Rand = new Rand("sum")
  println(model)
}
