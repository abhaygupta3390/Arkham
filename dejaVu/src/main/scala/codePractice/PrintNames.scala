package codePractice

object PrintNames {
  def main(args: Array[String]): Unit = {
    val T = scala.io.StdIn.readInt()
    val inputArray: Array[(String, Int, Int)] = Array.fill(T)(null, 0, 0)
    for (i<- 0 until T){
      val input = scala.io.StdIn.readLine().split(", ")
      val name = input(0)
      val rollNo = input(1).toInt
      val marks = input(2).toInt
      inputArray(i) = (name, rollNo, marks)
    }

    val b = inputArray
      .groupBy(_._3)
      .toSeq
      .sortBy(_._1)
      .reverse
      .map(x => (x._1, x._2.sortBy(_._1)))

    for(x<- b){
      for(y <- x._2){
        println(y._3 + " " + y._1)
      }
    }




  }

}
