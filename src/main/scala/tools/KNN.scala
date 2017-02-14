package tools

import java.util.concurrent.Executors
import scala.concurrent.duration._
import distances.Distance
import parsers.Parser
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}

object KNN {

  def search(data:Parser, queries:Parser, K:Int, distance:Distance): Array[Array[(Int,Double)]] = { // TODO do we need vectors aswell ?

    implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

    implicit object Ord extends Ordering[(Int,Double)] {
      def compare(x:(Int,Double), y:(Int,Double)) = x._2.compare(y._2)
    }
    var priorityQueues = new Array[mutable.PriorityQueue[(Int, Double)]](queries.size)

    for (p <- 0 until queries.size) {
      priorityQueues(p) = new mutable.PriorityQueue[(Int, Double)]()(Ord)
    }

    var loadedQueries = new Array[(Int, Array[Double])](queries.size)
    for (i <- 0 until loadedQueries.length) {
      loadedQueries(i) = queries.next
    }

    println("Building Structure")
    var progress = 0.0
    var percentile = data.size / 100


    while(data.hasNext) {
      var futures = new ArrayBuffer[Future[Unit]]
      var dataPoint = data.next
      for(i <- 0 until loadedQueries.length) {
        var q = loadedQueries(i)
        futures += Future {
          val pqEntry = (dataPoint._1, distance.measure(dataPoint._2, q._2))
          if(priorityQueues(i).size <= K) {
            priorityQueues(i).enqueue(pqEntry)
          }
          else {
            if(priorityQueues(i).head._2 > pqEntry._2) {
              priorityQueues(i).dequeue()
              priorityQueues(i).enqueue(pqEntry)
            }
          }
        }
      }
      Await.result(Future.sequence(futures), 20.seconds)
      progress+=1


      if(progress % percentile == 0) {
        print("\r"+((progress / data.size) * 100).toInt + "%")
      }
    }

    for{
      pq <- priorityQueues
    } yield pq.toArray
  }
}
