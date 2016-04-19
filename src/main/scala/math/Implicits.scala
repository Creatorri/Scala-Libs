package math

/**
  * Created by Torri on 4/15/2016.
  */
object Implicits {

  import scala.math.Fractional.Implicits._

  def scale[A: Fractional, B: Fractional](num: A, denom: B): A = num / denom.asInstanceOf[A]
}
