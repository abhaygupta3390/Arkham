package ProgFun

/**
  * Created by Abhay on 23/8/16.
  */
object PouringAssignment {

  def main(args: Array[String]): Unit = {
    val problem = new Pouring(Vector(4, 7, 19))
    println(problem.solution(17))
  }

}
