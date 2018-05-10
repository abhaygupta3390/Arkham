package codePractice

object RunningMedian {
  def main(args: Array[String]) {
    val n = scala.io.StdIn.readInt()
    val a = new Array[Int](n)
    for(i <- 0 until n) {
      a(i) = scala.io.StdIn.readInt()
    }

    val maxHeap = scala.collection.mutable.PriorityQueue[Int]()
    val minHeap = scala.collection.mutable.PriorityQueue[Int]()(Ordering.by(x => -1 * x))

    var extra = -1
    var insertElement = -1

    for (x <- a) {
      if (maxHeap.isEmpty || minHeap.isEmpty){
        insertElement = x
      } else if (x >= maxHeap.head && x <= minHeap.head){
        insertElement = x
      } else if (x < maxHeap.head){
        maxHeap.enqueue(x)
        insertElement = maxHeap.dequeue()
      } else if (x > minHeap.head){
        minHeap.enqueue(x)
        insertElement = minHeap.dequeue()
      }

      if (extra == -1){
        extra = insertElement
      } else {
        maxHeap.enqueue(Math.min(insertElement, extra))
        minHeap.enqueue(Math.max(insertElement, extra))
        extra = -1
      }


      extra match{
        case -1 => println((maxHeap.head.toDouble + minHeap.head.toDouble) / 2.0)
        case _ => println(extra.toDouble)
      }
    }
  }

}
