package codePractice

/**
  * Created by test on 26/5/17.
  */
object FinalPrice {
  def main (args: Array[String]): Unit = {
    val a = Array(5, 1, 3, 4, 6, 2)
    val b = finalPrice(a)
    b.foreach(println(_))
    println(a.sum - b.sum)
    for (i <- b.indices) {
      if (b(i) == 0) {
        print(i + " ")
      }
    }

  }

  def finalPrice(prices: Array[Int]): Array[Int] = {
    val disc: Array[Int] = Array.fill(prices.length)(0)
    val stack = new scala.collection.mutable.Stack[(Int, Int)]

    for (i <- prices.indices) {
      if (i == 0){
        stack.push((prices(i), i))
      } else {
        while (stack.nonEmpty && prices(i) <= peek(stack)._1) {
          val res = stack.pop()
          disc(res._2) = prices(i)
        }
        stack.push((prices(i), i))
      }

    }
    disc
  }

  def peek(stack: scala.collection.mutable.Stack[(Int, Int)]): (Int, Int) = {
    val a = stack.pop()
    stack.push(a)
    a
  }

}


