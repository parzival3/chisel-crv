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
      val payload = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))

      payload(0) #= (len + size)
    }

    val myPacket = new Packet
    myPacket.z.disable()
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

  }
}

