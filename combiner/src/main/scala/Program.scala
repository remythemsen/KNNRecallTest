import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Program extends App {
  val dataParser = Source.fromFile(args(0)).getLines()
  val qParser = Source.fromFile(args(1)).getLines()
  val writer = new BufferedWriter(new FileWriter(new File(args(2))))
  val newLine = System.getProperty("line.separator");

  var t = dataParser.next()
  for(i <- 0 until args(3).toInt) {
    if(t.split(" ")(0).toInt == i) {
      writer.write(t+newLine)
      if(dataParser.hasNext)
        t = dataParser.next()
    } else {
      println(i + " was != to " + t.split(" ")(0).toInt)
      writer.write(qParser.next()+newLine)
    }
  }
  writer.close()
}
