abstract class TreeNode[A]

case class Node[A] (value: A, left: TreeNode[A], right: TreeNode[A]) extends TreeNode[A]

case class Leaf[A]() extends TreeNode[A]

def nodesLevelOrder[A](t: TreeNode[A]): List[A] = {
  def helper(q: scala.collection.mutable.Queue[TreeNode[A]]) : List[A] = {
    if (q.isEmpty) {
      List()
    } else q.dequeue() match {
      case Leaf() => helper(q)
      case Node(v, l, r) =>
        q.enqueue(l)
        q.enqueue(r)
        v :: helper(q)
    }
  }
  helper(scala.collection.mutable.Queue(t))
}

def nodesInOrder[A](t : TreeNode[A]): List[A] = t match {
  case Node(v, l, r) => nodesInOrder(l) ::: List(v) ::: nodesInOrder(r)
  case Leaf() => List()
}

def nodesPreOrder[A](t : TreeNode[A]): List[A] = t match {
  case Node(v, l, r) => List(v) ::: nodesPreOrder(l) ::: nodesPreOrder(r)
  case Leaf() => List()
}

def nodesPostOrder[A](t : TreeNode[A]): List[A] = t match {
  case Node(v, l, r) => nodesPostOrder(l) ::: nodesPostOrder(r) ::: List(v)
  case Leaf() => List()
}

def mirror[A](t: TreeNode[A]): TreeNode[A] = t match {
  case Node(v, l, r) => Node(v, mirror(r), mirror(l))
  case Leaf() => Leaf()
}

def isMirrorStructure[A](t1: TreeNode[A], t2: TreeNode[A]): Boolean = t1 match {
  case Leaf() => t2 match {
    case Leaf() => true
    case Node(v, l, r) => false
  }
  case Node(v1, l1, r1) => t2 match {
    case Leaf() => false
    case Node(v2, l2, r2) => isMirrorStructure(l1, r2) && isMirrorStructure(l2, r1)
  }
}

def isSymmetric[A](t: TreeNode[A]): Boolean = t match {
  case Leaf() => true
  case Node(v, l, r) => isMirrorStructure(l, r)
}

val a = Node(1, Node(2, Node(5, Leaf(), Leaf()), Leaf()), Node(3, Leaf(), Node(7, Leaf(), Leaf())))

nodesInOrder(a)
nodesPreOrder(a)
nodesPostOrder(a)

isSymmetric(a)

mirror(a)

nodesInOrder(mirror(a))

nodesLevelOrder(a)

def height[A](t: TreeNode[A]): Int = t match {
  case Leaf() => 0
  case Node(v, l, r) => 1 + math.max(height(l), height(r))
}

height(a)

def diameter[A](t: TreeNode[A]): Int = t match {
  case Leaf() => 1
  case Node(v, l, r) => math.max(math.max(diameter(l), diameter(r)), height(l) + height(r) + 1)
}

diameter(a)

def maxWidth[A](t: TreeNode[A]): Int = {
  val countArray: Array[Int] = Array.fill(height(t)){0}
  def helper(t: TreeNode[A], level: Int): Unit = t match {
    case Leaf() =>
    case Node(v, l, r) =>
      countArray.update(level, countArray(level) + 1)
      helper(l, level+1)
      helper(r, level+1)
  }
  helper(t, 0)
  countArray.max
}

maxWidth(a)


def buildTreeFromInOrderAndPostOrder[A](inOrder: Array[A], preOrder: Array[A]): TreeNode[A] = {
  if (preOrder.isEmpty) {
    Leaf()
  } else if (preOrder.length == 1) {
    Node(preOrder(0), Leaf(), Leaf())
  } else {
    val rootVal = preOrder(0)
    val indexOfRootInOrder = inOrder.indexOf(rootVal)
    val inOrderLeft = inOrder.slice(0, indexOfRootInOrder)
    val inOrderRight = inOrder.slice(indexOfRootInOrder + 1, preOrder.length)
    val indexOfLastElementPreOrder = preOrder.indexOf(inOrderLeft.last)
    val preOrderLeft = preOrder.slice(1, indexOfLastElementPreOrder + 1)
    val preOrderRight = preOrder.slice(indexOfLastElementPreOrder + 1, inOrder.length)

    Node(rootVal, buildTreeFromInOrderAndPostOrder(inOrderLeft, preOrderLeft), buildTreeFromInOrderAndPostOrder(inOrderRight, preOrderRight))
  }
}

buildTreeFromInOrderAndPostOrder(Array(4, 2, 5, 1, 6, 3), Array(1, 2, 4, 5, 3, 6))
