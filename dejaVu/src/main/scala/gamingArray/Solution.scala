package gamingArray

object Solution {

  // Complete the gamingArray function below.
  def gamingArray(arr: Array[Int]): String = {
    var gamingArray = arr
    var b: Boolean = false
    while (!gamingArray.isEmpty){
      FindMax.array = gamingArray
      val maxIndex = FindMax.findMax(0, gamingArray)._1
//      val maxIndex = FindMax.findMaxIndex(gamingArray)
      gamingArray = gamingArray.take(maxIndex)
      b = !b
    }

    if (b){
      "BOB"
    } else {
      "ANDY"
    }
  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn


    val g = stdin.readLine.trim.toInt

    for (gItr <- 1 to g) {
      val arrCount = stdin.readLine.trim.toInt

      val arr = stdin.readLine.split(" ").map(_.trim.toInt)
      val result = gamingArray(arr)

      println(result)
    }
  }
}

