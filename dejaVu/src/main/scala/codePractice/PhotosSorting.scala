package codePractice

import java.text.SimpleDateFormat

object PhotosSorting {
  def main(args: Array[String]): Unit = {
    val S = "a.jpg, delhi, 2018-04-10 23:34:12\nb.jpg, delhi, 2018-03-09 21:24:56\nc.jpg, mumbai, 2017-09-09 12:12:04\nc.jpg, mumbai, 2017-09-09 12:12:15\nc.jpg, mumbai, 2017-09-09 12:12:03\nc.jpg, mumbai, 2017-09-09 12:12:25\nc.jpg, mumbai, 2017-09-09 12:12:12\nc.jpg, mumbai, 2017-09-09 12:12:02\nc.jpg, mumbai, 2017-09-09 12:12:01\nc.jpg, mumbai, 2017-09-09 12:12:45\nc.jpg, mumbai, 2017-09-09 12:12:12\nc.jpg, mumbai, 2017-09-09 12:12:24\nc.jpg, chand, 2017-09-09 12:12:09\nc.jpg, chand, 2017-09-09 12:12:08"
    function(S)
  }

  def function(S: String): Unit = {
    case class Photo(name: String, extension: String, city: String, ts: Long, index: Int)
    var i = 0
    val input =
      S
        .split("\n")
        .map(
          x => {
            i += 1
            Photo(
              x.split(", ")(0).split("\\.")(0),
              x.split(", ")(0).split("\\.")(1),
              x.split(", ")(1),
              getTimeStamp(x.split(", ")(2), "yyyy-MM-dd HH:mm:ss"),
              i
            )
          }
        )

    val cityMap = scala.collection.mutable.HashMap.empty[String, (Int, Int)]
    for (p <- input){
      if (!cityMap.contains(p.city))
        cityMap += (p.city -> (1, 1))
      else cityMap(p.city) = (cityMap(p.city)._1 + 1, cityMap(p.city)._2)
    }

    val sortedInput = input.sortBy(x => x.ts)

    val nameChanged = sortedInput
      .map(
        photo => {
          val maxPhotos = cityMap(photo.city)._1
          val index = cityMap(photo.city)._2
          val padding = maxPhotos.toString.length
          val number = s"%0${padding}d".format(index)
          val newName = photo.city + number + "." + photo.extension
          cityMap(photo.city) = (cityMap(photo.city)._1, cityMap(photo.city)._2 + 1)

          Photo(
            newName,
            photo.extension,
            photo.city,
            photo.ts,
            photo.index
          )
        }
      )

    val finalSort = nameChanged.sortBy(x => x.index)
    for(p <- finalSort){
      println(p.name)
    }
  }

  def getTimeStamp(date: String, dateFormat: String): Long = {
    try {
      val sdf = new SimpleDateFormat(dateFormat)
      val dt = sdf.parse(date)
      val time = dt.getTime
      time
    }
  }
}
