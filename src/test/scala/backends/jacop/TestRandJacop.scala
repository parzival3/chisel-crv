package backends.jacop

import org.scalatest.FlatSpec

class TestRandJacop extends FlatSpec with VerificationContext {
  behavior of "Rand variable in Jacop"
  it should "be able to declare a random variable and and a constraint" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      var size = new Rand("size", min, max)
      var len = new Rand("len", min, max)
      var z: crv.Constraint = len #>= size
      val x: crv.Constraint = len #<= size
      var y: crv.Constraint = len #> 4
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len + size)
    }

    val myPacket = new Packet
    myPacket.x.disable()
    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())
    println(myPacket.len)
    println(myPacket.size)
    myPacket.payload.foreach(println)
    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())
    myPacket.payload.foreach(println)
    println(myPacket.len)
    println(myPacket.size)
    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())
    myPacket.payload.foreach(println)
    println(myPacket.len)
    println(myPacket.size)
    myPacket.y.disable()
    myPacket.x.enable()
    myPacket.randomize
    assert(myPacket.len.value() <= myPacket.size.value())
    myPacket.debug()

  }
}

