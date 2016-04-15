package cs

/**
  * Created by Torri on 4/15/2016.
  */
object ListImplicits {

  implicit class SuperSeqPair[A](p: (Seq[A], Seq[A])) {
    def fold(func: (A, A, A) => A, startval: A): A = {
      if (p._1.isEmpty || p._2.isEmpty) {
        return startval
      }
      func(p._1.head, p._2.head, fold(func, startval)) // make so takes seqA and seqB as inputs
    }
  }
}
