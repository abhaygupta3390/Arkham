package QueensProblem

/**
  * Created by Abhay on 24/8/16.
  */

object NQueens {

  type Queen = (Int, Int)

  type Solutions = List[List[Queen]]

  def placeQueens(n: Int, boardSize: Int): Solutions = n match {
    case 0 => List(Nil)
    case _ => for {
      queenList <- placeQueens(n -1, boardSize)
      y <- 1 to boardSize
      currentQueen = (n, y)
      if isSafe(currentQueen, queenList)
    } yield currentQueen :: queenList
  }

  private def isSafe(queen: Queen, queenList: List[Queen]): Boolean = {
    queenList forall (!isAttacked(queen, _))
  }

  private def isAttacked(q1: Queen, q2: Queen): Boolean = {
    q1._1 == q2._1 ||
      q1._2 == q2._2 ||
      (q1._1 - q2._1).abs == (q1._2 - q2._2).abs
  }

  def printSolution(solution: List[Queen], boardSize: Int): Unit = {
    for (
      q <- solution ;
      i <- 1 to boardSize
    ) {
      if (q._2 == i) print("Q") else print("x")
      if (i == boardSize) println()
    }
    println()
  }

  def main(args: Array[String]): Unit = {
    val boardSize = 5
    val solutions: Solutions = placeQueens(boardSize, boardSize)
    solutions foreach (x => printSolution(x, boardSize))
  }
}
