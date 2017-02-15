import java.io.{File, FileOutputStream, ObjectOutputStream}
import java.util.concurrent.Executors

import distances.{Distance, Euclidean}
import parsers.RawParser

import scala.concurrent.duration._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext, Future}

case class Config(buildFromFile:String, queries:String, outPath:String, n:Int, qn:Int, knn:Int, distance:Distance)

object Program extends App {

  // LoadKNN.concatKNN()

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(12))

  val config = new Config(
      "data/descriptors-40000.data", // Data
      "data/queries-10.data", //queries-5-8069.data",       // Q File
      "data",                     // Out path
      39290,//20172529,          // N
      10,     //8063              // Queries
      30,                         // KNN
      Euclidean)                  // MEASURE

  val data = new RawParser(new File(config.buildFromFile))
  val queries = new RawParser(new File(config.queries))
  //val structure = new mutable.HashMap[Int, mutable.PriorityQueue[(Int, Double)]]
  val structure=new mutable.HashMap[Int,Array[(Int,Double)]]

  implicit object Ord extends Ordering[(Int,Double)] {
    def compare(x:(Int,Double), y:(Int,Double)) = x._2.compare(y._2)
  }

  println(queries.size)
  println("Building Structure")
  var progress = 0.0
  var percentile = config.n / 100

  var priorityQueues = new Array[mutable.PriorityQueue[(Int, Double)]](config.qn)
  for(p <- 0 until config.qn) {
    priorityQueues(p) = new mutable.PriorityQueue[(Int, Double)]()(Ord)
  }

  var loadedQueries = new Array[(Int, Array[Double])](config.qn)
  for(i <- 0 until loadedQueries.length) {
    loadedQueries(i) = queries.next
  }

  while(data.hasNext) {
    var futures = new ArrayBuffer[Future[Unit]]
    var dataPoint = data.next
    for(i <- 0 until loadedQueries.length) {
      var q = loadedQueries(i)
      futures += Future {
        val pqEntry = (dataPoint._1, config.distance.measure(dataPoint._2, q._2))
        if(priorityQueues(i).size <= config.knn) {
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
      println(((progress / config.n) * 100).toInt + "%")
    }
  }

  println("building table")
  for(i<- priorityQueues.indices) {
    val qTuples = priorityQueues(i).toArray.sortBy(x => x._2)
    structure += (qTuples.head._1 -> qTuples.slice(1, config.knn))
  }

  println("Saving structure to disk...")
  val oos = new ObjectOutputStream(new FileOutputStream(config.outPath+"/"+config.n+"_"+config.qn+"_"+config.distance.getClass.getSimpleName.substring(0,config.distance.getClass.getSimpleName.length-1)+".knnstructure"))
  oos.writeObject(structure)
  oos.close
  println("structure was saved..")
}
