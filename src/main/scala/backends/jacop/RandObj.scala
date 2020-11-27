package backends.jacop

import org.jacop.core.IntDomain
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

  val label = new DepthFirstSearch[Rand]

  private def search[A <: Rand](
    vars:      Iterable[A],
    heuristic: ComparatorVariable[A],
    indom:     Indomain[A]
  )(
    implicit m: ClassTag[A]
  ): SelectChoicePoint[A] =
    new SimpleSelect[A](vars.toArray, heuristic, indom)

  private val addLabelFun = new ThreadLocal[mutable.Buffer[DepthFirstSearch[_ <: org.jacop.core.Var]]]

  private def dfs(all: Boolean): DepthFirstSearch[Rand] = {

    label.setAssignSolution(true)
    label.setSolutionListener(new PrintOutListener[Rand]())
    if (all)
      label.getSolutionListener.searchAll(true)

    label
  }

  private def satisfySearch(
    select:   SelectChoicePoint[Rand],
    listener: SolutionListener[Rand],
    model:    Model
  ): Boolean = {
    model.imposeAllConstraints()

    val label = dfs(all = false)
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

class RandObj(r: Int = new util.Random().nextInt()) extends crv.RandObj {

  implicit val model: Model = getModel

  // We need a reference to the Parent RandomObj in order to enable or disable a constraint
  implicit val current: RandObj = this
  implicit def primitiveToConstraint(constraint: org.jacop.constraints.PrimitiveConstraint): Constraint =
    new Constraint(constraint)
  private var n = 0
  private val heuristic = new RandomComparator[Rand](r + n)
  private val listener = new SimpleSolutionListener[backends.jacop.Rand]
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
    problemVariables = model.vars.filter(x => x.isInstanceOf[Rand]).map(_.asInstanceOf[Rand]).toList
    problemVariables.foreach(x => domainDatabase += (x -> x.domain.cloneLight()))
    // listener = new SimpleSolutionListener[backends.jacop.Rand]
    n += 1
    model.setLevel(n)
    RandObj.satisfySearch(
      RandObj.search(problemVariables, heuristic, new IndomainRandom[Rand](new util.Random(r + n).nextInt())),
      listener,
      model
    )
  }
}
