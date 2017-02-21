package tools

import java.util.concurrent.Executors
import scala.concurrent.duration._
import distances.Distance
import parsers.Parser.Parser
import tools.DataPoint.NumericDataPoint
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}

object KNN {

  def search[A](data:Parser[NumericDataPoint[A]], queries:Parser[NumericDataPoint[A]], K:Int, distance:Distance, dataSetSize:Int) = {
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

    implicit object Ord extends Ordering[(NumericDataPoint[A], Double)] {
      def compare(x: (NumericDataPoint[A], Double), y: (NumericDataPoint[A], Double)) = x._2.compare(y._2)
    }

    println("Building Structure...")
    var progress = 0.0
    var percentile = dataSetSize / 100

    val pqs = new ArrayBuffer[(NumericDataPoint[A], mutable.PriorityQueue[(NumericDataPoint[A], Double)])]
    while(queries.hasNext) {
      pqs+= Tuple2(queries.next.get, new mutable.PriorityQueue[(NumericDataPoint[A], Double)]()(Ord))
    }

    // Run over dataset once
    while(data.hasNext) {
      val dp = data.next
      val futures: ArrayBuffer[Future[Unit]] = for {
        pq <- pqs
      } yield {
        Future {
          val distFromQ = pq._1.computeDistance(dp.get)
          // Update pq if better tuple is found
          if(!pq._1.equalTo(dp.get)) {
            if (pq._2.size < K) {
              pq._2.enqueue((dp.get, distFromQ))
            } else if (pq._2.head._2 > distFromQ) {
              pq._2.dequeue()
              pq._2.enqueue((dp.get, distFromQ))
            }
          }
        }
      }
      // Await all pq's update
      Await.result(Future.sequence(futures), 20.seconds)

      progress += 1
      if (progress % percentile == 0) {
        print("\r" + ((progress / dataSetSize) * 100).toInt + "%")
      }
    }
    print("\r" + 100.toString + "%\n")
    println("Returning resultset...")
    val resultSets = new ArrayBuffer[(NumericDataPoint[A], Array[(NumericDataPoint[A], Double)])]()
    for(pq <- pqs) {
      resultSets += Tuple2(pq._1,pq._2.toArray)
    }
    resultSets
  }
}
