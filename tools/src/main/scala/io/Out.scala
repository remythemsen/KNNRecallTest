package io

import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable.ArrayBuffer

object Out {

  def writeToFile(outputFilePath:String, resultSets:ArrayBuffer[(String, String, String, String, String, Double, Double, Double)]) : Unit = {
    // Time and Date
    val now = new java.util.Date
    val time = new java.text.SimpleDateFormat("yyyy-MM-dd_HH.mm").format(now)

    val file = new File(outputFilePath+"-"+time)
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

    //sb.append("Optimal Average Closest Point Distance from Queries: "+resultSets.head._7.toString+"\n")


    bw.write(sb.toString)

    for(resultSet <- resultSets) {
      val sb = new StringBuilder
      // filename
      sb.append(resultSet._1+" ")
      // qfilename
      sb.append(resultSet._2+" ")
      // K
      sb.append(resultSet._3+" ")
      // Measure
      sb.append(resultSet._4+" ")
      // Numeric Type
      sb.append(resultSet._5+" ")

      // Recall
      sb.append(resultSet._6.toString+"\n")

      // Avg Closest Point distance found from queries
      //sb.append(resultSet._8.toString+"\n")

      bw.write(sb.toString)
    }

    bw.close
  }
}
