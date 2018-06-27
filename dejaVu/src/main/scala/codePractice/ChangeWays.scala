package codePractice

object ChangeWays {
  var res = 0
  val map = scala.collection.mutable.HashMap[Long,Int]()
  map.put(0, 1)

  // Complete the getWays function below.
  def getWays(n: Long, c: Array[Long]): Long = {
    count(n, c.length, c)
  }

  private def count(n: Long, l: Int, c : Array[Long]): Long = {
    for (i <- 0 until l; j <- c(i) to n){
      map.put(j, map.getOrElse(j, 0) + map.getOrElse(j - c(i), 0))
    }

    map(n)

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val nm = stdin.readLine.split(" ")

    val n = nm(0).trim.toInt

    val m = nm(1).trim.toInt

    val c = stdin.readLine.split(" ").map(_.trim.toLong)
    // Print the number of ways of making change for 'n' units using coins having the values given by 'c'

    val ways = getWays(n, c)
    print(ways)

  }
}