abstract class TreeNode[A]

case class Node[A] (value: A, left: TreeNode[A], right: TreeNode[A]) extends TreeNode[A]

case class Leaf[A]() extends TreeNode[A]

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

