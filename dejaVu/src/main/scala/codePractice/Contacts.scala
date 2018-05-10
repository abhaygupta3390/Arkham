package codePractice

object Contacts {

  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    val map = scala.collection.mutable.Map[String, Int]()
    for (i <- 1 to n){
      val s = scala.io.StdIn.readLine().split(" ")
      s(0) match {
        case "find" =>
          println(find(s(1), map))
        case "add" =>
          add(s(1), map)
      }
    }
  }

  def find(s: String, map: scala.collection.mutable.Map[String, Int]): Int = {
    if(map.contains(s))
      map(s)
    else
      0
  }

  def add(s: String, map: scala.collection.mutable.Map[String, Int]): Unit = {
    val l = s.length
    for (i <- 0 until l) {
      val str = s.substring(0, i + 1)
      if (map.contains(str)){
        map(str) = map(str) + 1
      } else {
        map += (str -> 1)
      }
    }
  }

}
