package backends.jacop
import org.jacop.core.Var
import org.jacop.search.ComparatorVariable

class RandomComparator[T <: Var](seed: Int) extends ComparatorVariable[T] {
  val rand = new util.Random(seed)
  override def compare(left: Float, `var`: T): Int = {
    rand.nextInt(1)
  }

  override def compare(leftVar: T, rightVar: T): Int = {
    rand.nextInt(1)
  }

  override def metric(`var`: T): Float = {
    rand.nextInt(10).toFloat
  }
}
