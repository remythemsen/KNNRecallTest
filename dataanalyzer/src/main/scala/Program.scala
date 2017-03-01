import java.io.{BufferedWriter, File, FileWriter}

import io.Parser.{RawParserDouble, ReducedParserDouble}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Program extends App {
  val n = 1008263
  val data = new RawParserDouble(Source.fromFile(new File(args(0))).getLines()) // Data file
  val outDir = args(1)
  val dimensions = 4096

  // Vector
  var minVectorLength = 0.0
  var maxVectorLength = 0.0
  var sumVectorLength = 0.0
  var avgVectorLength = 0.0
  var stdDevVectorLength = 0.0
  lazy val vSums = ArrayBuffer[Double]()

  // Components
  var minCpmLength = 0.0
  var maxCpmLength = 0.0
  var sumCpmLength = 0.0
  var avgCpmLength = 0.0

  var progress = 0.0
  val percentile = n / 100



  while(data.hasNext) {
    val tuple = data.next.head.get
    val vSum = Math.sqrt(tuple._2.map(x => Math.pow(x, 2)).sum)
    vSums += vSum
    if(vSum < minVectorLength) {
      minVectorLength = vSum
    }
    if(vSum > maxVectorLength) {
      maxVectorLength = vSum
    }
    sumVectorLength+=vSum

    // components
    for(c <- tuple._2) {
      if(c < minCpmLength) minCpmLength = c
      if(c > maxCpmLength) maxCpmLength = c
      sumCpmLength+=c
    }
    progress+=1.0
    if(progress % percentile == 0) {
      println(((progress / n)*100).toInt +"%")
    }
  }
  avgCpmLength = sumCpmLength/(n*dimensions)
  avgVectorLength = sumVectorLength/n
  stdDevVectorLength = {
    val recallVariance = {
      var tmp = 0.0
      for (s <- vSums) {
        tmp += (s - avgVectorLength) * (s - avgVectorLength)
      }
      tmp / vSums.size
    }
    Math.sqrt(recallVariance)
  }


  // Test #1, what is the Max, Min, and Average of a component in a vector?


  // Test #2, what is the standard dev of the average of component


  // Time and Date
  val now = new java.util.Date
  val time = new java.text.SimpleDateFormat("yyyy-MM-dd_HH.mm").format(now)

  val file = new File(outDir+"dataanalysis-"+time)
  val bw = new BufferedWriter(new FileWriter(file))
  // System Information
  val sb = new StringBuilder
  sb.append("OS: ")
  sb.append(System.getProperty("os.name")+", ")
  sb.append(System.getProperty("os.version")+", ")
  sb.append(System.getProperty("os.arch")+"\n")
  sb.append("JVM: ")
  sb.append(System.getProperty("java.vendor")+", ")
  sb.append(System.getProperty("java.version")+"\n")
  sb.append("CPU: ")
  sb.append(System.getenv("PROCESSOR_IDENTIFIER")+", ")
  sb.append("Cores: "+Runtime.getRuntime().availableProcessors()+"\n")
  sb.append("\n")

  sb.append("File: \n")
  sb.append(args(0)+"\n\n")

  // Formatting results
  sb.append("Results: \n")
  sb.append("Component analysis:\n")
  sb.append("Min component length: "+minCpmLength+"\n")
  sb.append("Max component length: "+maxCpmLength+"\n")
  sb.append("Average component length: "+avgCpmLength+"\n\n")
  sb.append("Vector analysis:\n")
  sb.append("Min vector length: "+minVectorLength+"\n")
  sb.append("Max vector length: "+maxVectorLength+"\n")
  sb.append("Average vector length: "+avgVectorLength+"\n")
  sb.append("Standard dev: "+stdDevVectorLength+"\n\n")


  bw.write(sb.toString)

  bw.close



}


