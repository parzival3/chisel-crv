package backends.jacop

import crv.ConstraintGroup
import org.scalatest.FlatSpec

class TestRandJacop extends FlatSpec with VerificationContext {
  behavior.of("Rand variable in Jacop")
  it should "be able to declare a random variable and and a constraint" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      var size = new Rand("size", min, max)
      var len = new Rand("len", min, max)
      var z:       crv.Constraint = len #>= size
      val x:       crv.Constraint = len #<= size
      var y:       crv.Constraint = len #> 4
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len + size)
    }

    val myPacket = new Packet
    myPacket.x.disable()
    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())

    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())

    assert(myPacket.randomize)
    assert(myPacket.len.value() >= myPacket.size.value())

    myPacket.y.disable()
    myPacket.x.enable()
    myPacket.randomize
    assert(myPacket.len.value() <= myPacket.size.value())
  }

  it should "be able to subtract two Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len - size)
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0).value() == myPacket.len.value - myPacket.size.value())
  }

  it should "be able to add two Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len + size)
      payload(1) #= (len + 4)
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0).value() == myPacket.len.value + myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value + 4)
  }

  it should "be able to divide two Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len.div(size))
      payload(1) #= (len.div(4))
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0).value() == myPacket.len.value / myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value / 4)
  }

  it should "be able to multiply two Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len * size)
      payload(1) #= (len * 4)
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0).value() == myPacket.len.value * myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value * 4)
  }

  it should "be able to constraint the reminder of Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      val size = new Rand("size", min, max)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 100))
      payload(0) #= (len.mod(size))
      payload(1) #= (len.mod(4))
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0).value() == myPacket.len.value % myPacket.size.value())
    assert(myPacket.payload(1).value() == myPacket.len.value % 4)
  }

  it should "be able to constraint the exponential of Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 10
      val size = new Rand("size", 2, 3)
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 10))
      payload(0) #= (len ^ size)
      payload(1) #= (len ^ 3)
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0).value() == math.pow(myPacket.len.value.toDouble, myPacket.size.value().toDouble).toInt)
    assert(myPacket.payload(1).value() == math.pow(myPacket.len.value.toDouble, 3).toInt)
  }

  it should "be able to constraint less or equal then  Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 10
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 10))
      payload(0) #<= len
      payload(1) #<= 3
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0) <= myPacket.len)
    assert(myPacket.payload(1) <= 3)
  }

  it should "be able to constraint less then  Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 10
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 10))
      payload(0) #< len
      payload(1) #< 3
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0) < myPacket.len)
    assert(myPacket.payload(1) < 3)
  }

  it should "be able to constraint gather or equal than of Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 10
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 10))
      payload(0) #>= len
      payload(1) #>= 3
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0) >= myPacket.len)
    assert(myPacket.payload(1) >= 3)
  }

  it should "be able to constraint gather than of Rand var" in {
    class Packet extends RandObj(3) {
      val min = 1
      val max = 10
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", 1, 10))
      payload(0) #> len
      payload(1) #> 3
    }
    val myPacket = new Packet
    myPacket.randomize
    assert(myPacket.payload(0) > myPacket.len)
    assert(myPacket.payload(1) > 3)
  }

  it should "be able to add Constraint Groups" in {

    class Packet extends RandObj(3) {
      val min = 1
      val max = 100
      val len = new Rand("len", min, max)
      val payload: Array[Rand] = Array.tabulate(11)(i => new Rand("byte[" + i + "]", min, max))
      val cgroup: ConstraintGroup = new ConstraintGroup {
        payload(0) #> len
        payload(1) #> 98
      }

      val negc: crv.Constraint = payload(1) #< 98
      negc.disable()
    }

    val myPacket = new Packet
    assert(myPacket.randomize)
    assert(myPacket.payload(0) > myPacket.len)
    assert(myPacket.payload(1) > 98)
    myPacket.cgroup.disable()
    myPacket.cgroup.enable()
    assert(myPacket.randomize)
    assert(myPacket.payload(1) < 98)
  }
}
