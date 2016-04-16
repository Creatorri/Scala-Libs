/**
  * Created by Torri on 4/16/2016.
  */
object Tester {
  def main(args: Array[String]) {
    import math.Vector._
    val v = new RectVector[Double](0.1, 0.5, 9)
    val pos = new RectVector[Double](4, 5, 8)
    println(v dot pos)
  }
}
