package codePractice

/**
  * Created by Abhay on 9/8/17.
  */
object IsChange {
  def main(args: Array[String]) = {
    val T = scala.io.StdIn.readInt()
    val a: Array[Int] = scala.io.StdIn.readLine().split(" ").map(x => x.toInt)

    println(helper(T, a))

  }

  private def helper(T: Int, a: Array[Int]): Boolean = {
    if (T == 0) {
      return true
    } else if (T < 0) {
      return false
    }

    var res: Boolean = false

    for (e <- a){
      res = res || helper(T - e, a)
    }

    res
  }
}
