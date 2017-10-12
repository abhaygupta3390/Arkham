package codePractice

/**
  * Created by test on 15/6/17.
  */
object Palindromes {

  def main(args: Array[String]): Unit = {
    val s = scala.io.StdIn.readLine()
    printAllPalindromes(s)
  }

  def printAllPalindromes(s: String): Unit = {
    for(x <- 0 until s.length){
      printPalindromes(s, x, x)
      printPalindromes(s, x, x+1)
    }
  }

  def printPalindromes(s: String, x: Int, y: Int): Unit = {
    if (x >= 0 && y < s.length && s.charAt(x) == s.charAt(y)){
      println(s.substring(x, y + 1))
      printPalindromes(s, x-1, y+1)
    }
  }

}
