package math

/**
  * Created by Torri on 4/15/2016.
  */
object Matrix {
  def identity[A: Numeric](dim: Int): Matrix[A] = ???

  class Matrix[A](rows: Seq[A]*)(implicit num: Numeric[A]) {

  }
}
