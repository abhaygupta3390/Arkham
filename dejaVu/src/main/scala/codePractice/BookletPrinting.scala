package codePractice

/**
  * Created by test on 12/10/17.
  */
object BookletPrinting {
  def main(args: Array[String]): Unit = {
    val pages = scala.io.StdIn.readInt()
    printBooklet(pages)
  }

  private def printBooklet(pages: Int): Unit = {
    if (pages == 0) {
      return
    }
    val sheets = if (pages % 4 == 0) {
      pages / 4
    } else {
      pages / 4 + 1
    }

    for (i <- 1 to sheets){
      val maxPages = if (pages %4 ==0) {
        pages
      } else {
        pages + 4 - (pages % 4)
      }
      val frontLeft = maxPages - 2 * (i - 1)
      val frontRight = 2 * i - 1
      val backLeft = 2 * i
      val backRight = maxPages - 2 * (i - 1) - 1
      val frontLeftPage: Any = if (frontLeft > pages){
        "blank"
      } else {
        frontLeft
      }
      val frontRightPage: Any = if (frontRight > pages){
        "blank"
      } else {
        frontRight
      }
      val backLeftPage: Any = if (backLeft > pages){
        "blank"
      } else {
        backLeft
      }
      val backRightPage: Any = if (backRight > pages){
        "blank"
      } else {
        backRight
      }
      if(!(frontLeftPage == "blank" && frontRightPage== "blank")){
        println("S%s,front,%s,%s".format(i, frontLeftPage, frontRightPage))
      }
      if(!(backLeftPage == "blank" && backRightPage == "blank")){
        println("S%s,back,%s,%s".format(i, backLeftPage, backRightPage))
      }

    }
  }
}
