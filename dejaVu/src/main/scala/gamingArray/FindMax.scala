package gamingArray

object FindMax {

  var array: Array[Int] = _

  def findMax(offset: Int, arr: Array[Int]): (Int, Int) = {
    if (arr.length == 1){
      return (offset, arr(0))
    }
    val size = arr.length
    val divisionCount = Math.ceil(size.toDouble / 2).toInt
    val leftArray = arr.take(divisionCount)
    val rightArray = arr.drop(divisionCount)

    compareTuple(findMax(offset,leftArray), findMax(offset + divisionCount, rightArray))

  }

  def compareTuple(pair1: (Int, Int), pair2: (Int, Int)): (Int, Int) = {
    if (pair1._2 >= pair2._2){
      pair1
    } else {
      pair2
    }
  }

  def findMaxIndex(arr: Array[Int]): Int = {
    var max = Integer.MIN_VALUE
    var index = -1
    for (i <- arr.indices){
      if (arr(i) > max){
        max = arr(i)
        index = i
      }
    }
    index
  }

}
