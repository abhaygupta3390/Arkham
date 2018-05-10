package codePractice;

public class isBST {

    public static void main(String[] args) {
        //TODO: Input
    }

    boolean checkBST(Node root) {
        return checkBSTWithRange(root, Integer.MAX_VALUE, Integer.MIN_VALUE);
    }

    boolean checkBSTWithRange(Node root, int max, int min) {
        if (root == null) {
            return true;
        } else if (root.data < max && root.data > min){
            return checkBSTWithRange(root.left, root.data, min) && checkBSTWithRange(root.right, max, root.data);
        } else {
            return false;
        }

    }
}

class Node {
    int data;
    Node left;
    Node right;
}
