package math

/**
  * Created by Torri on 4/15/2016.
  */

import cs.ListImplicits

object Vector {

  import scala.math.Integral.Implicits._

  private def scale[B: Integral](num: B, denom: B) = num / denom

  /**
    * One vector to rule them all
    *
    * @tparam A Numeric type
    */
  trait SuperVector[@specialized A] {

    /**
      * Dim is the number of dimensions that this vector has.
      */
    val dim: Int

    /**
      * This is the length of the vector. Plain and simple.
      */
    val length: Double

    /**
      * Gets the rectangular form of the vector
      *
      * @return rectangular vector
      */
    def toRect(): RectVector[A]

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
    def *(scalar: A): SuperVector[A]

    /**
      * Divides a vector by a scalar
      *
      * @param scalar A number
      * @return The resultant
      */
    def /(scalar: A): SuperVector[A]

    /**
      * Finds the dot product of the two vectors.
      * Essentially finds how much of the first vector is in the direction of the second.
      *
      * @param other The vector to be dot producted with
      * @return A number
      */
    def dot(other: SuperVector[A]): A

    def cross(other: SuperVector[A]): SuperVector[A]
  }

  /**
    * Poly-Dimensional Rectangular vector
    *
    * @param values amount in each direction
    * @tparam A Numeric type
    */
  class RectVector[@specialized A](val values: A*)(implicit num: Numeric[A]) extends SuperVector[A] {
    /**
      * This is the length of the vector. Plain and simple.
      */
    override lazy val length: Double = scala.math.sqrt(num.toDouble(values.map(x => num.times(x, x)).sum))
    /**
      * Dim is the number of dimensions that this vector has.
      */
    override val dim: Int = values.length

    override def cross(other: SuperVector[A]): SuperVector[A] = ???

    /**
      * Divides a vector by a scalar
      *
      * @param scalar A number
      * @return The resultant
      */
    override def /(scalar: A): SuperVector[A] = {
      new RectVector[A](values.map(x => scale[A](x, scalar)): _*)
    }

    /**
      * Adds two vectors
      *
      * @param other What is being added.
      * @return The resultant
      */
    override def +(other: SuperVector[A]): SuperVector[A] = ???

    /**
      * Finds the dot product of the two vectors.
      * Essentially finds how much of the first vector is in the direction of the second.
      *
      * @param other The vector to be dot producted with
      * @return A number
      */
    override def dot(other: SuperVector[A]): A = {
      val rectother = other.toRect()
      if (other.dim != dim) {
        throw new Exception("Wrong Vector Dimensions in Dot Product! Must be the same.")
      }
      ListImplicits.fold[A]((values, rectother.values), (a: A, b: A, c: A) => num.plus(a, num.plus(b, c)), num.zero)
    }

    /**
      * Gets the rectangular form of the vector
      *
      * @return rectangular vector
      */
    override def toRect(): RectVector[A] = ???

    /**
      * Subtracts two vectors
      *
      * @param other What is to be subtracted
      * @return The resultant
      */
    override def -(other: SuperVector[A]): SuperVector[A] = ???

    /**
      * Multiplies a vector by a scalar
      *
      * @param scalar A number
      * @return The resultant
      */
    override def *(scalar: A): SuperVector[A] = ???
  }

}