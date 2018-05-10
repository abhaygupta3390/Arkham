package codePractice

object Uber {
  def main(args: Array[String]) {
      val N = scala.io.StdIn.readInt()
      val input = Array.fill(2* N)((0,0))
      for(i <- 0 until N){
        val str = scala.io.StdIn.readLine().split(" ").map(_.toInt)
        input(2*i) = (str(0),1)
        input(2*i + 1) = (str(1), -1)
      }
      val sortedInput = input.sortWith((a,b) => a._1 < b._1)
      var ctr = 0
      var count = 0
      var start = Integer.MIN_VALUE
      for (a <- sortedInput){
        if (count == 0) {
          start = a._1
        }
        count = count + a._2
        if(count == 0) {
          ctr += a._1 - start + 1
        }
      }
      println(ctr)

  }

}
