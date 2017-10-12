package codePractice

/**
  * Created by Abhay on 30/3/17.
  */
object Histogram {
  def main(args: Array[String]): Unit = {

    val a = scala.io.StdIn.readLine().split(" ").map(_.toInt)

    val lookupArea = Array.fill[Int](a.length)(0)
    val lookupLastIndex = Array.fill[Int](a.length)(0)

    for (j <- a.indices){
      maxArea(j)
    }

    println(lookupArea(a.length -1))

    def maxArea(i: Int): Unit = {
      if (i == 0) {
        lookupArea(0) = a(0)
        lookupLastIndex(0) = 1
      } else {
        if (lookupLastIndex(i - 1) == i - 1) {
          lookupArea(i) = Math.max(lookupArea(i-1) + Math.min(a(i), a(i-1)), a(i))
          lookupLastIndex(i) = i
        } else {
          lookupArea(i) = Math.max(lookupArea(i - 1), a(i))
          lookupLastIndex(i) =
            if (lookupArea(i - 1) > a(i)) {
              lookupLastIndex(i-1)
            } else {
              i
            }
        }
      }
    }
  }
}
