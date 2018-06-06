package codePractice

object PhoneMap {

  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    val phoneMap = scala.collection.mutable.Map[String, Int]()
    for (i <- 0 until n) {
      val entry = scala.io.StdIn.readLine().trim.split(" ")
      val name = entry(0)
      val number = entry(1).toInt
      phoneMap += (name -> number)
    }

    val queries = Iterator
      .continually(Console.readLine)
      .takeWhile(_ != null)
    for (q <- queries){
      if (phoneMap.contains(q)){
        println("%s=%s".format(q,phoneMap(q)))
      } else {
        println("Not found")
      }
    }
  }
}
