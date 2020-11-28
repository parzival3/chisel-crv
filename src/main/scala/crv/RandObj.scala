package crv

trait RandObj {

  /**
    * Randomize the current object
    * @return Boolean the result of the current randomization
    */
  def randomize: Boolean

  def preRandomize(): Unit = {}

  def postRandomize(): Unit = {}
}
