package math

/**
  * Created by Torri on 4/15/2016.
  */

object Vector {

  /**
    * One vector to rule them all
    *
    * @tparam A Numeric type
    */
  trait SuperVector[@specialized(Int, Double) A] {

    /**
      * Number of dimensions that the vector has.
      */
    val dim: Int

    /**
      * This is the length of the vector. Plain and simple.
      */
    val length: Double

    /**
      * Gets unit vector from the current vector
      *
      * @return Unit vector
      */
    def getUnit(): SuperVector[A] = this / length

    /**
      * Adds two vectors
      *
      * @param other What is being added.
      * @return The resultant
      */
    def +(other: SuperVector[A]): SuperVector[A]

    /**
      * Subtracts two vectors
      *
      * @param other What is to be subtracted
      * @return The resultant
      */
    def -(other: SuperVector[A]): SuperVector[A]

    /**
      * Multiplies a vector by a scalar
      *
      * @param scalar A number
      * @return The resultant
      */
    def *[B: Fractional](scalar: B): SuperVector[A]

    /**
      * Divides a vector by a scalar
      *
      * @param scalar A number
      * @return The resultant
      */
    def /[B: Fractional](scalar: B): SuperVector[A]

    /**
      * Finds the dot product of the two vectors.
      * Essentially finds how much of the first vector is in the direction of the second.
      *
      * @param other The vector to be dot producted with
      * @return A number
      */
    def dot(other: SuperVector[A]): A

    /**
      * Cross product of two vectors
      *
      * @param other the cross vector
      * @return the product vector
      */
    def cross(other: SuperVector[A]): SuperVector[A]

    /**
      * Rotates the vector
      *
      * @param angle radians to rotate by in each direction
      * @return rotated vector
      */
    def rotate(angle: Double*): SuperVector[A]
  }

  /**
    * Vector in Cartesian form
    *
    * @param values amount in each unit direction
    * @tparam A Numeric type
    */
  abstract class SuperRectVector[@specialized(Int, Double) A](val values: A*)(implicit num: Numeric[A]) extends SuperVector[A] {

    /**
      * This is the length of the vector. Plain and simple.
      */
    lazy val length = scala.math.sqrt(values.map(x => num.toDouble(x) * num.toDouble(x)).sum)
    /**
      * Number of dimensions that the vector has.
      */
    val dim = values.length

    /**
      * Finds the dot product of the two vectors.
      * Essentially finds how much of the first vector is in the direction of the second.
      *
      * @param other The vector to be dot producted with
      * @return A number
      */
    def dot(other: SuperRectVector[A]) = {
      if (other.dim != dim) {
        throw new Exception("Wrong Vector Dimensions in Dot Product! Must be the same.")
      }
      import cs.Implicits._
      fold[A]((values, other.values), (a: A, b: A, c: A) => num.plus(a, num.plus(b, c)), num.zero)
    }
  }
}
