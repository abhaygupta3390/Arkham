package codePractice

object SnakesAndLadders {
  val ladders = scala.collection.mutable.HashMap[Int, Int]()
  val snakes = scala.collection.mutable.HashMap[Int, Int]()
  val seen = scala.collection.mutable.HashMap[Int, Int]()

  def quickestWayUp(): Int = {
    println("In helper")
    helper(1, 0)
  }

  private def helper(currentPos: Int, movesCompleted: Int): Int = {
//    println("currentPos: %s   MovesCompleted: %s".format(currentPos, movesCompleted))
    if(currentPos < 100 && seen.contains(100) && seen(100) < movesCompleted){
//      println("greater than res")
      return Integer.MAX_VALUE
    } else if(currentPos <= 100 && seen.contains(currentPos) && seen(currentPos) < movesCompleted){
//      println("used map to eliminate")
      return Integer.MAX_VALUE
    } else if (currentPos <= 100){
//      println("inserted in map currentPos: %s   MovesCompleted: %s".format(currentPos, movesCompleted))
      seen.put(currentPos, movesCompleted)
    }

    if (currentPos == 100){
      movesCompleted
    } else if (currentPos > 100){
      Integer.MAX_VALUE
    } else if (ladders.contains(currentPos)){
      helper(ladders(currentPos), movesCompleted)
    } else if (snakes.contains(currentPos)){
      helper(snakes(currentPos), movesCompleted)
    } else {
      Array(
        helper(currentPos + 6, movesCompleted + 1),
        helper(currentPos + 5, movesCompleted + 1),
        helper(currentPos + 4, movesCompleted + 1),
        helper(currentPos + 3, movesCompleted + 1),
        helper(currentPos + 2, movesCompleted + 1),
        helper(currentPos + 1, movesCompleted + 1)
      ).min
    }
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn
    val t = stdin.readLine.trim.toInt

    for (tItr <- 1 to t) {
//      println("IterationNo: " +tItr)
      ladders.clear()
      snakes.clear()
      seen.clear()
      val n = stdin.readLine.trim.toInt
//      println("n: " + n)

      for (i <- 0 until n) {
        val l = stdin.readLine.split(" ").map(_.trim.toInt)
        ladders.put(l(0), l(1))
      }

      val m = stdin.readLine.trim.toInt
//      println("m: " + m)

      for (i <- 0 until m) {
        val s = stdin.readLine.split(" ").map(_.trim.toInt)
        snakes.put(s(0), s(1))
      }
//      println("hkjhkjhkjhkjh")

      val result = quickestWayUp()

      println(result)
    }
  }
}
