package codePractice

/**
  * Created by Abhay on 30/3/17.
  */
object DPFactorial {
  def main(args: Array[String]) {
    val factMap = scala.collection.mutable.HashMap[Int, Int]()
    factMap.update(0, 1)
    val t = readInt()
    val a = Array.fill[Int](t)(0)
    for (i <- 0 until t){
      a(i) = readInt()
    }

    for (j <- 0 until t) {
      println(fact(a(j)))
    }

    def fact(n: Int): Int = {
      if (factMap.contains(n)){
        factMap(n)
      } else {
        factMap.update(n, Math.floorMod(n * fact(n-1),1000000007))
        factMap(n)
      }
    }
  }
}
