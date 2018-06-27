package codePractice

object Stats {
  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    val a = scala.io.StdIn.readLine().trim.split(" ").map(_.toInt)
    val sortedArray = a.sorted
    val mean: Double = a.sum.toDouble / n.toDouble
    val map = scala.collection.mutable.HashMap[Int, Int]()
    val median: Double = if (n % 2 == 1){
      sortedArray(n/2)
    } else {
       (sortedArray(n/2 - 1) + sortedArray(n/2)).toDouble / 2.0
    }

    for (b <- a){
      if (map.contains(b)){
        map(b) = map(b) + 1
      } else {
        map.put(b , 1)
      }
    }
    val mode = map.toSeq.sortWith((x, y) => x._1 < y._1).sortWith((x, y) => x._2 > y._2).head._1

    println(mean)
    println(median)
    println(mode)

  }

}
