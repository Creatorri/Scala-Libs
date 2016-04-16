package cs

/**
  * Created by Torri on 4/15/2016.
  */
object ListImplicits {
  def fold[A](p: (Seq[A], Seq[A]), func: (A, A, A) => A, startval: A): A = {
    if (p._1.isEmpty || p._2.isEmpty) {
      return startval
    }
    func(p._1.head, p._2.head, fold((p._1.tail, p._2.tail), func, startval))
  }
}
