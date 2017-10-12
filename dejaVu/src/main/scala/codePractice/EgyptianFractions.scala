package codePractice

/**
  * Created by test on 12/10/17.
  */
object EgyptianFractions {
  def main(args: Array[String]): Unit = {
    val temp = scala.io.StdIn.readLine().split(" ").map(x =>  x.toInt)
    egypt(temp(0), temp(1))
  }

  private def egypt(num: Int, den: Int): Unit = {
    if (num == 0 || den == 0){
      return
    }
    if (den % num == 0) {
      print(den/num)
      return
    }

  }
}
