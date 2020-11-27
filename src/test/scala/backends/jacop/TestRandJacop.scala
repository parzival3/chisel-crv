package backends.jacop

import org.scalatest.FlatSpec

class TestRandJacop extends FlatSpec with VerificationContext {
  behavior of "Rand variable in Jacop"
  it should "be able to declare a random variable and and a constraint" in {
    class Packet extends RandObj {
      val min = 1
      val max = 100
      var size = new Rand("size", min, max)
      var len = new Rand("len", min, max)
      var z: Constraint = size #>= len
      var y: Constraint = len #>= size
      val payload = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 4))
    }

    val myPacket = new Packet
    myPacket.z.disable()
    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())
//    println("First Test")
//    myPacket.z.enable()
//    myPacket.y.disable()
//    assert(myPacket.randomize)
//    assert(myPacket.size.value() >= myPacket.len.value())
//    println("Second Test")
//    assert(myPacket.randomize)
//    assert(myPacket.size.value() >= myPacket.len.value())
//    println("Third Test")
  }
}

//object SendMoreMoney extends App with jacop {
//  val s = new IntVar("s", 0, 9)
//  val m = new IntVar("m", 0, 9)
//
//  val fd = Array(s, m)
//
//  alldifferent(fd)
//
//  val z = m #< s
//  val model = getModel
//  z.removeConstraint()
//  model.addChanged(z)
//  model.constr -= z
//  println(model.constr)
//
//
//  val result = satisfy(search(fd, input_order, new IndomainRandom),
//    printSol())
//
//  statistics
//
//  def printSol() = () => {
//    print("Solution: ")
//    for (v <- fd)
//      print(v + " ")
//    println
//  }
//}
//
//
