case class NodeGeneric[T](itemName: T, next: NodeGeneric[T])

class LinkedStack[T] {
  var first: NodeGeneric[T] = _

  def isEmpty: Boolean = {
    first == null
  }

  def push(element: T) = {
    val oldFirst = first
    first = NodeGeneric(element, oldFirst)
  }

  def pop(): T = first match {
    case null => throw new Exception("Underflow: Stack Empty, cant pop")
    case _ =>
      val popValue = first.itemName
      first = first.next
      popValue
  }
}

val a = new LinkedStack[Int]

a.isEmpty
a.push(3)
a.push(4)
a.push(5)
a.push(6)
a.push(7)
a.pop()
a.pop()

def reverseString(input: String): String = {
  val a = new LinkedStack[Char]
  var i: Int = 0
  while (i < input.length ) {
    a.push(input.charAt(i))
    i += 1
  }
  var res: String = ""
  while(!a.isEmpty) {
    res += a.pop()
  }
  res
}

reverseString("abcvd")

def isParenthesesBalanced(input: String): Boolean = {
  val a = new LinkedStack[Char]
  var i = 0
  try {
    while(i < input.length) {
      def c = input.charAt(i)
      if (Array('{', '[', '(').contains(c)) {
        a.push(c)
      } else if (c == ')' && !(a.pop()=='(')){
        return false
      } else if (c == '}' && !(a.pop()=='{')){
        return false
      } else if (c == ']' && !(a.pop()=='[')){
        return false
      }
      i += 1
    }
  } catch {
    case e: Throwable => return false
  }
  true
}

isParenthesesBalanced("(())")
isParenthesesBalanced("))((")
isParenthesesBalanced("({)}")
isParenthesesBalanced("")
