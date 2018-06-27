package codePractice

object QuartilesRange {

  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    val x = scala.io.StdIn.readLine().split(" ").map(_.trim.toInt)
    val f = scala.io.StdIn.readLine().split(" ").map(_.trim.toInt)

    val arr: IndexedSeq[Int] = for (i <- 0 until n; j <- 0 until f(i)) yield {
      x(i)
    }
    val sortedArr = arr.sorted
    val leftArr = sortedArr.take(sortedArr.length / 2)
    val rightArr = sortedArr.takeRight(sortedArr.length / 2)

    val qr = median(rightArr) - median(leftArr)
    println(qr)
  }

  private def median(arr: IndexedSeq[Int]): Double = {
    val l = arr.length
    if (l % 2 == 0){
      (arr(l/2 -1) + arr(l/2)).toDouble / 2.0
    } else {
      arr(l/2).toDouble
    }
  }
}
