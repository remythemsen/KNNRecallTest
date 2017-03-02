import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import io.Parser.RawParserDouble

import scala.collection.mutable
import scala.io.Source

/**
  * Created by remeeh on 02-03-2017.
  */
object queryFinder extends App {

  val queryPoints = Source.fromFile("data/queries-1m-100.data").getLines
  val qpMap = mutable.HashMap[Int, Boolean]()

  while (queryPoints.hasNext) {
    qpMap += (queryPoints.next.toInt -> true)
  }

  val input = new RawParserDouble(Source.fromFile("data/descriptors-1-million.data").getLines)

  val qpOutPut = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("data/descriptors-1-million-100.queries")))
  while(input.hasNext) {
    val sb = new StringBuilder
    val t = input.next
    // if point is a querypoint
    if (qpMap.contains(t.head.get._1)) {
      // Build ID line
      val orgId = {
        val zeroC = 10 - t.head.get._1.toString.length
        var zeroes = {
          val sb = new StringBuilder
          for(i <- 0 until zeroC) {
            sb.append("0")
          }
          sb
        }
        zeroes.append(t.head.get._1).toString
      }
      sb.append("#objectKey messif.objects.keys.AbstractObjectKey " + orgId)
      sb.append("\n")
      for (c <- t.head.get._2) {
        sb.append(c + " ")
      }
      sb.append("\n")

      qpOutPut.write(sb.toString())
    }

  }
  qpOutPut.close


}
