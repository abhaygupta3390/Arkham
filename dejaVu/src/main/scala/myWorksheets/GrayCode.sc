// Gray Code

def appendToEach(s: String, l: List[String]): List[String] = l match {
  case List() => List()
  case x :: xs => (s + x) :: appendToEach(s, xs)
}

def reverse(l: List[String]): List[String] = l match {
  case List() => List()
  case x :: xs => reverse(xs) ::: List(x)
}

def genGray(n: Int): List[String] = n match {
  case 0 => List()
  case 1 => List("0", "1")
  case _ => appendToEach("0", genGray(n-1)) :::  appendToEach("1", reverse(genGray(n-1)))
}

genGray(0)
genGray(1)
genGray(2)
genGray(3)
genGray(4)