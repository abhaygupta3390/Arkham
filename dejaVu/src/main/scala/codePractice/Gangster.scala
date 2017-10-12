package codePractice

/**
  * Created by test on 12/10/17.
  */
object Gangster {
  def main(args: Array[String]): Unit = {
    val T = scala.io.StdIn.readInt()
    for (i <- 1 to T) {
      runTestCase()
    }
  }

  private def runTestCase() = {
    val temp = scala.io.StdIn.readLine().split(" ").map(x =>  x.toInt)
    val N = temp(0)
    val X = temp(1)
    val S = Array.fill(N)(0)
    val E = Array.fill(N)(0)
    for (i <- 0 until N) {
      val inputQuery = scala.io.StdIn.readLine().split(" ").map(x =>  x.toInt)
      S(i) = inputQuery(0)
      E(i) = inputQuery(1)
    }

    for(i <- 0 until N){
      for(j <- i + 1 until N){
        if(E(j) < E(i)){
          var temp = E(i)
          E(i) = E(j)
          E(j) = temp
          temp = S(i)
          S(i) = S(j)
          S(j) = temp
        }
        else if(E(j) == E(i) && S(j) > S(i)){
          val temp = S(i)
          S(i) = S(j)
          S(j) = temp
        }
      }
    }

    val arr: Array[Int] = Array.fill(N)(0)

    var res = 0
    for(i <- 0 until N){
      var flag = false
      for(j <- 0 to i){
        if(E(j) >= S(i)){
          arr(j) += 1
          if(arr(j) == X){
            flag = true
          }
        }
      }
      if(flag){
        res += 1
        for(j <- 0 to i){
          if(E(j) >= S(i)){
            arr(j) -= 1
          }
        }
      }
    }
    println(res)
  }
}
