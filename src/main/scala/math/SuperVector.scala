package math

/**
  * Created by Torri on 4/15/2016.
  */

object Vector {

  class SuperVector[A](values: A*)(implicit num: Numeric[A]) {
    val n = values.length

    def dot(other: SuperVector[A]): A = {
      if (other.n != n) {
        throw new Exception("Wrong Vector Dimensions in Dot Product! Must be the same.")
      }

    }
  }

}
