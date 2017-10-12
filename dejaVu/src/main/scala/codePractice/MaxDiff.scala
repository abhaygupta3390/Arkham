package codePractice

/**
  * Created by Abhay on 9/8/17.
  */
object MaxDiff {
  def main(args: Array[String]) = {
    val n = scala.io.StdIn.readInt()
    val a: Array[Int] = scala.io.StdIn.readLine().split(" ").map(x => x.toInt)
    val q = scala.io.StdIn.readInt()

    for (i <- 1 to q) {
      val inputQuery = scala.io.StdIn.readLine().split(" ").map(x =>  x.toInt)
      val l = inputQuery(0)
      val r = inputQuery(1)
      println(getQueryResult(l, r, a))
    }
  }

  private def getQueryResult(l: Int, r: Int, a: Array[Int]): Int = {
    var max = a(l)
    var diff = a(l) - a(l + 1)

    for (i <- l to r) {
      if (max - a(i) > diff) {
        diff = max - a(i)
      }
      if (a(i) > max){
        max = a(i)
      }
    }

    diff
  }

}
