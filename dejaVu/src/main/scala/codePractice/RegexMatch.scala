package codePractice

/**
  * Created by Abhay on 10/5/17.
  */
object RegexMatch {
  def main (args: Array[String]): Unit = {
    val a = regexMatch("cccc", "*ccc")
    println(a)
  }

  def regexMatch(str: String, regex: String, index1: Int = 0, index2: Int = 0): Boolean = {
    println("\"%s\" \"%s\" %s %s".format(str, regex, index1, index2))

    if (index1 == str.length && index2 == regex.length) {
      println("success")
      true
    } else if (index1 >= str.length ^ index2 >= regex.length) {
      println("index exceeded")
      false
    } else if (regex.charAt(index2) != '.' && regex.charAt(index2) != '*' && str.charAt(index1) == regex.charAt(index2)) {
      println("normal char match")
      regexMatch(str, regex, index1 + 1, index2 + 1)
    } else if (regex.charAt(index2) == '*') {
      println("found *")
      regexMatch(str, regex, index1, index2 + 1) || regexMatch(str, regex, index1 + 1, index2 + 1)
    } else if (regex.charAt(index2) == '.'){
      println("found .")
      regexMatch(str, regex, index1 + 1, index2 + 1)
    } else {
      false
    }
  }

}
