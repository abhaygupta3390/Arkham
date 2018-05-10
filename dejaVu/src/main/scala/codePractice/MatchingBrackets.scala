package codePractice

object MatchingBrackets {

  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    for(i <- 0 until n){
      val s = scala.io.StdIn.readLine()
      println(isMatched(s))
    }
  }

  def isMatched(s: String): String = {
    val stack = scala.collection.mutable.Stack[Char]()
    for (c <- s){
      if (c == '(' || c == '[' || c == '{'){
        stack.push(c)
      } else if ((c == ')' && stack.pop() != '(') || (c == ']' && stack.pop() != '[') || (c == '}' && stack.pop() != '{')){
        return "NO"
      }
    }

    if (stack.isEmpty)
      "YES"
    else
      "NO"

  }
}
