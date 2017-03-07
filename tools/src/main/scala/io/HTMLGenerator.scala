package io

import java.io._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 12/24/16.
  */
class HTMLGenerator(globalSb:StringBuilder) extends App {

  private def writeToFile(filepath:String, content:String) = {
    val file = new File(filepath)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }
  // For each Q, add a section
  def addSection(d:(String, ArrayBuffer[(Int, Array[(Int, Double)])])): Unit = {
    globalSb.append("<div>")
    globalSb.append("<h4>"+d._1+":</h4>")


    for(p <- d._2) {
        addRow(p._1, p._2)
    }
    globalSb.append("</div>")
  }

  def addRow(q:Int, a:Array[(Int,Double)]): Unit= {
    // Append whole result set for q'i
    globalSb.append("<div style=\"display:inline-flex\">")
    // Adding qp
    globalSb.append("<div class=\"imgBox\">")
    globalSb.append(imgTag(q.toString))
    globalSb.append("<p>querypoint</p>")
    globalSb.append("</div>")
    for(i <- a.indices) {
      globalSb.append("<div class=\"imgBox\">")
      globalSb.append(imgTag(a(i)._1.toString))
      globalSb.append("<p>"+a(i)._2+"</p>")
      globalSb.append("</div>")
    }
    globalSb.append("</div>")
  }

  def outPut(path:String) : Unit = {

    val sb = new StringBuilder
    // HEADER
    sb.append("<!doctype html>")
    sb.append("\n<html lang=\"en\">")
    sb.append("\n<head>")
    sb.append("\n<meta chartset=\"utf-8\">")
    sb.append("\n<title>ARKNNISS DEMO</title>")
    // triple double-quotes because of scala string interpolation bug

    sb.append("<style>.imgBox {float:left;width:auto;margin-bottom:10px;margin-right:10px;background-color:#dddddd;} img{width:130px;height:150px;margin-right:15px;}</style>")
    sb.append("\n</head>")
    // BODY
    sb.append("\n<body>")

    sb.append("<div>")
    sb ++= globalSb // Adding all results
    sb.append("</div>")

    sb.append("\n</body>")
    sb.append("\n</html>")


    writeToFile(path, sb.mkString)
  }
  private def imgTag(id:String) = {
    val orgId = {
      val zeroC = 10 - id.length
      var zeroes = {
        val sb = new StringBuilder
        for(i <- 0 until zeroC) {
          sb.append("0")
        }
        sb
      }
      zeroes.append(id).toString
    }
    // triple double-quotes because of scala string interpolation bug
    s"""<img src=\"http://disa.fi.muni.cz/profimedia/images/$orgId\" />"""
  }
}
