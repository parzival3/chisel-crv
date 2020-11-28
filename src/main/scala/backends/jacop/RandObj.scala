package backends.jacop

import org.jacop.core.IntDomain
import org.jacop.scala.{Model}
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
    indom:     Indomain[A]
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
    model:    Model
  ): Boolean = {
    model.imposeAllConstraints()

    val label = dfs[A](all = false)
    label.setAssignSolution(true)
    label.setPrintInfo(false)
    addLabel(label)
    label.setSolutionListener(listener)
    val lbList = label.getSolutionListener
    lbList.searchAll(false)
    label.labeling(model, select)
  }

  private def addLabel(label: DepthFirstSearch[_ <: Rand]): Unit = {
    val b = addLabelFun.get()
    if (b != null) b += label
  }
}

class RandObj(r: Int = new util.Random().nextInt(), implicit val model: Model = new Model) extends crv.RandObj {

  // We need a reference to the Parent RandomObj in order to enable or disable a constraint
  implicit val current: RandObj = this
  implicit def primitiveToConstraint(constraint: org.jacop.constraints.PrimitiveConstraint): Constraint =
    new Constraint(constraint)
  private var nOfCalls = 0
  private val heuristic = new RandomComparator[Rand](r)
  private val listener = new SimpleSolutionListener[backends.jacop.Rand]
  private val domainDatabase = mutable.Map[Rand, IntDomain]()
  private var problemVariables = List[Rand]()
  private var initialize = false

  def resetDomains(): Unit = {
    domainDatabase.foreach(k => k._1.domain.setDomain(k._2))
  }

  def debug(): Unit = {
    problemVariables.foreach(println)
  }

  /**
    * Randomize the current object
    *
    * @return Boolean the result of the current randomization
    */
  override def randomize: Boolean = {
    nOfCalls += 1
    if (!initialize) {
      // TODO: remove this requirement
      require(nOfCalls == 1)
      initialize = true
      problemVariables = model.vars.filter(x => x.isInstanceOf[Rand]).map(_.asInstanceOf[Rand]).toList
      problemVariables.foreach(x => domainDatabase += (x -> x.domain.cloneLight()))
    }
    resetDomains()
    RandObj.satisfySearch(
      RandObj.search(problemVariables, heuristic, new IndomainRandom[Rand](nOfCalls)),
      listener,
      model
    )
  }
}
