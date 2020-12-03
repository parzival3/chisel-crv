
![Github actions](https://github.com/parzival3/chisel-crv/workflows/Scala%20CI/badge.svg) [![codecov](https://codecov.io/gh/parzival3/chisel-crv/branch/develop/graph/badge.svg?token=1UWX7OCVTD)](https://codecov.io/gh/parzival3/chisel-crv)

# Chisel CRV
Chisel CRV is a project that aims to mimic the functionality of SystemVerilog constraint programming and integrates them into [chisel-tester2](https://github.com/ucb-bar/chisel-testers2).
Chisel CRV combines a Constraint Satisfactory Problem Solver, with some helper classes to create and use random objects inside your tests.

## Comparison
### System Verilog

```systemverilog
class frame_t;
rand pkt_type ptype;
rand integer len;
randc bit [1:0] no_repeat;
rand bit  [7:0] payload [];
// Constraint the members
constraint legal {
  len >= 2;
  len <= 5;
  payload.size() == len;
}
```

### Chisel CRV / jacop backend
```scala
import backends.jacop._

class Frame extends RandObj(new Model) {
  val pkType: Rand = new Rand(0, 3)
  val len: Rand = new Rand(0, 10)
  val noRepeat: Randc = new Randc(0, 1)
  val payload: RandArr = new RandArr(0, 7, _model)

  val legal: ConstraintGroup = new ConstraintGroup {
    len #>= 2
    len #<= 5
    payload.size #= len
  }
}
```

## CSP Solver
Based on the ideas of the book [Artificial Intelligence A Modern Approach](https://www.pearson.com/us/higher-education/program/Russell-Artificial-Intelligence-A-Modern-Approach-4th-Edition/PGM1263338.html),
Is a combination of  **BacktrackSearching** and **Constraint Propagation**.
The pseudocode for the algorithm used can be found [here](http://aima.cs.berkeley.edu/algorithms.pdf).
The CSP solver and the relative components are stored in the `csp` package.

## Random Objects
Random objects can be created by extending the RandObj trait. This class accepts one parameter which is a Model. A model
correspond to a database in which all the random variable and constraint declared inside the RandObj are stored.
```scala
import backends.jacop._
class Frame extends RandObj(new Model)
```
A model can be initialized with a seed `new Model(42)`, which allows the user to specify reproducible tests.

### Random Fields
A random field can be added to a `RandObj` by declaring a Rand variable.
```scala
  val len: Rand = new Rand(0, 10)
```

Continuous random variable can be added by declaring a `Randc` field inside a `RandObj`
```scala
  val noRepeat: Randc = Randc(0, 1)
```

### Constraints
Each variable can have one or multiple constraint. Constraint relations are usually preceded by the `#` symbol.
```scala
len #>= 2
```
In the previous block of code we are specifying that the variable `len` can only take values that are grater then 2. 
Each constraint can be assigned to a variable and  enabled or disabled at any time during the test
```scala
val lenConstraint = len #> 2
[....]
lenConstraint.disable()
[....]
lenConstraint.enable()
```

Constraint can also be grouped together in a `ConstraintGrup` and the group itself can be enabled or disabled.

```scala
val legal: ConstraintGroup = new ConstraintGroup {
  len #>= 2
  len #<= 5
  payload.size #= len
}
[...]
legal.disable()
[...]
legal.enable()
```

By default, constraints and constraints groups are enabled when they are declared. 


The list of operator used to construct constraint is the following:
`#<`, `#<=`, `#>`, `#>=`,`#=`, `div`, `*`, `mod`, `+`, `-`, `#\=`, `^`, `in`, `inside`

It is also possible to declare conditional constraints with constructors like `IfThen` and `IfThenElse`.
```scala
val constraint1: crv.Constraint = ifThenElse(len #= 1)(payload.size #= 3)(payload.size #= 10)
```

### Usage
As in SystemVerilog, each random class exposes a method called `randomize()` this method automatically solves the
constraint specified in the class and assign to each random filed a random value. The method returns `true`  only if the
CSP found a set of values that satisfy the current constraints.
```scala
val myPacket = new Frame(new Model)
assert(myPacket.randomize)
```

## Online Resources
- [jacop](https://github.com/radsz/jacop)
- [choco-solver](https://github.com/chocoteam/choco-solver)
- [optaplanner](https://github.com/kiegroup/optaplanner) 
- [Constraint Solvingwithout Surprises in Object-ConstraintProgramming Languages](http://www.vpri.org/pdf/tr2015002_oopsla15_babelsberg.pdf)
- [Exact Cover](https://garethrees.org/2015/11/17/exact-cover/) Series of 3 articles about constraint programming
- [CLP(FD) Prolog library about constraints](http://www.pathwayslms.com/swipltuts/clpfd/clpfd.html) 

