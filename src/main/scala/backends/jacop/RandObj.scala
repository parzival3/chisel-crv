package backends.jacop

import org.jacop.core.{Domain, IntDomain}
import org.jacop.scala.{getModel, Model}
import org.jacop.search.{
  ComparatorVariable,
  DepthFirstSearch,
  Indomain,
  IndomainRandom,
  PrintOutListener,
  SelectChoicePoint,
  SimpleSelect,
  SimpleSolutionListener,
  SolutionListener
}

import scala.collection.mutable
import scala.reflect.ClassTag

object RandObj {

  private def search[A <: Rand](
    vars:      Iterable[A],
    heuristic: ComparatorVariable[A],
    indom:     Indomain[A],
  )(
    implicit m: ClassTag[A]
  ): SelectChoicePoint[A] =
    new SimpleSelect[A](vars.toArray, heuristic, indom)

  private val addLabelFun = new ThreadLocal[mutable.Buffer[DepthFirstSearch[_ <: org.jacop.core.Var]]]

  private def dfs[A <: Rand](all: Boolean): DepthFirstSearch[A] = {
    val label = new DepthFirstSearch[A]

    label.setAssignSolution(true)
    label.setSolutionListener(new PrintOutListener[A]())
    if (all)
      label.getSolutionListener.searchAll(true)

    label
  }

  private def satisfySearch[A <: Rand](
    select:   SelectChoicePoint[A],
    listener: SolutionListener[A],
    model: Model
  ): Boolean = {
    model.imposeAllConstraints()

    val label = dfs[A](all = true)
    label.setAssignSolution(false)
    label.setPrintInfo(false)
    addLabel(label)
    label.setSolutionListener(listener)
    val lbList = label.getSolutionListener
    lbList.searchAll(true)
    lbList.recordSolutions(true)

    label.labeling(model, select)
  }

  private def addLabel(label: DepthFirstSearch[_ <: Rand]): Unit = {
    val b = addLabelFun.get()
    if (b != null) b += label
  }
}

class RandObj(r: Int = new util.Random().nextInt()) extends crv.RandObj {

  implicit val model: Model = getModel

  // We need a reference to the Parent RandomObj in order to enable or disable a constraint
  implicit val current: RandObj = this
  implicit def primitiveToConstraint(constraint: org.jacop.constraints.PrimitiveConstraint): Constraint =
    new Constraint(constraint)
  var initialized = false
  private var currentSolution = 0
  private var listener = new SimpleSolutionListener[backends.jacop.Rand]
  private val domainDatabase = mutable.Map[Rand, IntDomain]()
  private var problemVariables = List[Rand]()

  def resetDomains(): Unit = {
    domainDatabase.foreach(k => k._1.domain.setDomain(k._2))
  }

  /**
    * Randomize the current object
    *
    * @return Boolean the result of the current randomization
    */
  override def randomize: Boolean = {
    if (!initialized) {
      initialized = true
      problemVariables = model.vars.filter(x => x.isInstanceOf[Rand]).map(_.asInstanceOf[Rand]).toList
      problemVariables.foreach(x => domainDatabase += (x -> x.domain.cloneLight()))
      listener = new SimpleSolutionListener[backends.jacop.Rand]
      listener.solutions = Array.ofDim[Domain](100, 100)

      RandObj.satisfySearch(
        RandObj.search(problemVariables, null, new IndomainRandom[Rand](r + model.level)),
        listener, model
      )

      listener.assignSolution(model, currentSolution)
    } else if (initialized && currentSolution < listener.solutions.length) {
      currentSolution += 1
      resetDomains()
      domainDatabase.foreach(k => k._1.domain.setDomain(k._2))
      println(domainDatabase)
      listener.assignSolution(model, currentSolution)
      true
    } else {
      false
    }
  }
}
