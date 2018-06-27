package codePractice

  object SigFig extends App {

    val aa = getTrades("abc")

    for(a <- aa){
      println(a)
    }

    def getFriendsListForUser(userId: String) = List("user-1","user-2","user-3")


    def getTradeTransactionsForUser(userId: String): List[String] = {
      userId match {
        case "user-1" =>
          List(
            "2017-10-06,SELL,GOOG",
            "2017-09-28,BUY,YHOO"
          )

        case "user-2" =>
          List(
            "2017-10-05,BUY,GOOG",
            "2017-10-04,BUY,YHOO",
            "2017-10-04,BUY,MSFT"
          )
        case "user-3" =>
          List(
            "2017-10-03,SELL,GOOG",
            "2017-10-03,BUY,YHOO",
            "2017-10-01,SELL,MSFT",
            "2017-09-13,BUY,FB"

          )
        case _ => List()
      }
    }

    def getTrades(userId: String): List[String] = {

      val friendList = getFriendsListForUser(userId)
      val trades = for (f <- friendList;
                        t <- getTradeTransactionsForUser(f)
      ) yield t
      def toTuple(x: String): (String, Int) = {
        val a = x.split(",")
        val trade = a(1) match {
          case "BUY" => 1
          case "SELL" => -1
        }
        (a(2), trade)
      }

      val tradeTuples = trades.map(toTuple)



      val b = tradeTuples
        .groupBy(_._1)
        .map(x => (x._1, x._2.map(_._2).sum))
        .filter(_._2 != 0)
        .map(x =>
         if (x._2 > 0){
           (x._2, "BUY", x._1)
         } else{
           (x._2, "SELL", x._1)
         }
       ).toList
       .sortWith((x,y) => x._3 < y._3).sortWith((x,y) => x._1 > y._1)
       .map(x => x._1 + "," + x._2 + "," + x._3)

      b

    }
  }
