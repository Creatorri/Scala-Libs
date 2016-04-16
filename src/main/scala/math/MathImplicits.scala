package math

/**
  * Created by Torri on 4/15/2016.
  */
object MathImplicits {

  import scala.math.Integral.Implicits._

  def scale[B: Integral](num: B, denom: B) = num / denom
}
