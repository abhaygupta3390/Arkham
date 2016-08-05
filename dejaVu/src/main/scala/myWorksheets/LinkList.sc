abstract class Link[A]

case class NonEmptyLink[A](value: A, next: Link[A]) extends Link[A]

case class Empty[A]() extends Link[A]

def insert[A](value: A, pos: Int, ll: Link[A]): Link[A] = pos match {
  case 0 => NonEmptyLink(value, ll)
  case _ => ll match {
    case Empty() => throw new Exception("linked list is too small")
    case NonEmptyLink(v, n) => NonEmptyLink(v, insert(value, pos-1, n))
  }
}

val a = NonEmptyLink(1, NonEmptyLink(2, NonEmptyLink(3, NonEmptyLink(5, Empty()))))

insert(4, 2, a)

def deletePosition[A](position: Int, ll: Link[A]): Link[A] = position match {
  case 0 => ll match {
    case Empty() => throw new Exception("No Node to delete")
    case NonEmptyLink(v, n) => n
  }
  case _ => ll match {
    case Empty() => throw new Exception("No Node to delete")
    case NonEmptyLink(v, n) => NonEmptyLink(v, deletePosition(position-1, n))
  }
}

deletePosition(2, a)

def deleteValues[A](value: A, ll: Link[A]): Link[A] = ll match {
  case Empty() => Empty()
  case NonEmptyLink(v, n) =>
    if (v == value) {
      deleteValues(value, n)
    } else {
      NonEmptyLink(v, deleteValues(value, n))
    }
}

deleteValues(1, a)

def lengthLinkList[A](ll: Link[A]): Int = ll match {
  case Empty() => 0
  case NonEmptyLink(v, n) => 1 + lengthLinkList(n)
}

lengthLinkList(a)

def getLinkFromIndex[A](index: Int, ll: Link[A]): Link[A] = index match {
  case 0 => ll match {
    case Empty() => throw new Exception("no element at index")
    case NonEmptyLink(v, n) => ll
  }
  case _ => ll match {
    case Empty() => throw new Exception("no element at index")
    case NonEmptyLink(v, n) => getLinkFromIndex(index - 1, n)
  }
}

def getValueAtIndex[A](index: Int, ll: Link[A]): A = index match {
  case 0 => ll match {
    case Empty() => throw new Exception("no element at index")
    case NonEmptyLink(v, n) => v
  }
  case _ => ll match {
    case Empty() => throw new Exception("no element at index")
    case NonEmptyLink(v, n) => getValueAtIndex(index - 1, n)
  }
}

getLinkFromIndex(2, a)

def valExists[A](value: A, ll: Link[A]): Boolean = ll match {
  case Empty() => false
  case NonEmptyLink(v, n) => (v == value) || valExists(value, n)
}

valExists(1, a)
valExists(3, a)

def swap[A](index1: Int, index2: Int, ll: Link[A]): Link[A] = {
  if (index2 == index1) {
    throw new Exception("same indices")
  } else if (index2 < index1) {
    swapOrderedIndex(index2, index1, ll)
  } else {
    swapOrderedIndex(index1, index2, ll)
  }
}


// ToDo: Functional Approach for swap
def swapOrderedIndex[A](index1: Int, index2: Int, ll: Link[A]): Link[A] = ???



def getMiddleValue[A](ll: Link[A]): A = {
  def helper(llSlow: Link[A], llFast: Link[A]): A = llFast match {
    case Empty() => llSlow match {
      case Empty() => throw new Exception("Something Wrong")
      case NonEmptyLink(v, n) => v
    }
    case NonEmptyLink(v, n) => llSlow match {
      case Empty() => throw new Exception("Something Wrong")
      case NonEmptyLink(v2, n2) => n match {
        case Empty() => helper(llSlow, Empty())
        case NonEmptyLink(vf, nf) => helper(n2, nf)
      }
    }
  }
  helper(ll, ll)
}

getMiddleValue(a)

def reverse[A](ll: Link[A]): Link[A] = {
  def helper(remList: Link[A], accReversedList: Link[A]): Link[A] = remList match {
    case Empty() => accReversedList
    case NonEmptyLink(v, n) => helper(n, NonEmptyLink(v, accReversedList))
  }
  helper(ll, Empty())
}

reverse(a)

val l = List(2, 4, 3)
l.sortWith(_ < _)

def listToLinkedList[A](l: List[A]): Link[A] = l match {
  case List() => Empty()
  case x :: xs => NonEmptyLink(x, listToLinkedList(xs))
}

def mergeSortedLinkedLists[A](ll1: Link[A], ll2: Link[A])(implicit f: A => Ordered[A]): Link[A] = ll1 match {
  case Empty() => ll2
  case NonEmptyLink(v1, n1) => ll2 match {
    case Empty() => ll1
    case NonEmptyLink(v2, n2) =>
      if (v1 < v2) {
        NonEmptyLink(v1, mergeSortedLinkedLists(n1, ll2))
      } else {
        NonEmptyLink(v2, mergeSortedLinkedLists(ll1, n2))
      }
  }
}

mergeSortedLinkedLists(listToLinkedList(List(1, 3, 7, 9, 16)), listToLinkedList(List(2, 4, 5, 10, 11, 18)))

def isPalindrome[A](ll: Link[A]): Boolean = ll match {
  case Empty() => true
  case NonEmptyLink(v, Empty()) => true
  case NonEmptyLink(v, n) => (v == getValueAtIndex(lengthLinkList(n) - 1, n)) && isPalindrome(deletePosition(lengthLinkList(n)-1, n))
}

isPalindrome(listToLinkedList(List(1, 3, 7, 9, 16)))
isPalindrome(listToLinkedList(List(1, 3, 7, 3, 1)))
isPalindrome(listToLinkedList(List(1, 3, 7, 8, 3, 1)))
isPalindrome(listToLinkedList(List(1, 3, 7, 7, 3, 1)))
isPalindrome(listToLinkedList(List(1)))
isPalindrome(listToLinkedList(List()))


def pairwiseSwap[A](ll: Link[A]): Link[A] = ll match {
  case Empty() => Empty()
  case NonEmptyLink(v, Empty()) => ll
  case NonEmptyLink(v, NonEmptyLink(v1, n1)) =>
    NonEmptyLink(v1, NonEmptyLink(v, pairwiseSwap(n1)))
}

pairwiseSwap(listToLinkedList(List()))
pairwiseSwap(listToLinkedList(List(1, 2)))
pairwiseSwap(listToLinkedList(List(1, 2, 4)))
pairwiseSwap(listToLinkedList(List(1, 2, 4, 5, 7, 8)))

def deleteAlternate[A](ll: Link[A]): Link[A] = ll match {
  case Empty() => Empty()
  case NonEmptyLink(v, Empty()) => ll
  case NonEmptyLink(v, NonEmptyLink(v2, n2)) => NonEmptyLink(v, deleteAlternate(n2))
}

deleteAlternate(listToLinkedList(List(1, 2, 4, 5, 7, 8)))

def append[A](ll1: Link[A], ll2: Link[A]): Link[A] = ll1 match {
  case Empty() => ll2
  case NonEmptyLink(v, n) => NonEmptyLink(v, append(n, ll2))
}


def reverseInGroups[A](groupSize: Int, ll: Link[A]): Link[A] = {
  def helper(k: Int, remList: Link[A], accList: Link[A]): Link[A] = remList match {
    case Empty() => accList
    case NonEmptyLink(v, n) => k match {
      case 0 => append(accList, helper(groupSize, remList, Empty()))
      case _ => helper(k-1, n, append(NonEmptyLink(v, Empty()), accList))
    }
  }
  helper(groupSize, ll, Empty())
}

reverseInGroups(3, listToLinkedList(List(1, 2, 3, 4, 5, 6, 7, 8)))

def rotateLinkedList[A](k: Int, ll: Link[A]): Link[A] = ll match {
  case Empty() => Empty()
  case NonEmptyLink(v, n) => k match {
    case 0 => ll
    case _ => rotateLinkedList(k-1, append(n, NonEmptyLink(v, Empty())))
  }
}

rotateLinkedList(3, listToLinkedList(List(1, 2, 3, 4, 5, 6, 7, 8)))
