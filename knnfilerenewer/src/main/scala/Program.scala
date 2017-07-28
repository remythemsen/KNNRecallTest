import java.io.{File, FileInputStream, ObjectInputStream}

import scala.collection.mutable

object Program extends App {
  var knnFilePath = args(0)
  var outFilePath = args(1)
  println("Loading map ")
  val map = loadKNNObject(new File(knnFilePath))
  //var test = map.get("2791")


  println("Done")
  writeToFile(map, new File(outFilePath))


  def writeToFile(map:mutable.HashMap[Int,Array[(Int, Double)]], file:File) : Unit = {
    ???
    println("Writing map to new file")

    println("Finished writing map to file")
  }

  def loadKNNObject(file:File) : mutable.HashMap[Int, Array[(Int, Double)]] = {
    val objReader = new ObjectInputStream(new FileInputStream(file))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int, Array[(Int, Double)]]]
    objReader.close()
    hashMap
  }
}
