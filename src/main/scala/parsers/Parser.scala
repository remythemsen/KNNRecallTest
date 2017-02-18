package parsers

import java.io.File

import distances.{Cosine, Distance, Euclidean}
import tools.TestCase

import scala.reflect._
import scala.io.Source

trait Parser {
  def next : (Int, Array[Double])
  var data : Iterator[String] = _
  var fileName : String = _
  var size:Int = _
  def hasNext:Boolean

}

/**
  * Parses file of format:
  *
  * ############ (x49) 00002131
  * 0.1, 0.2, 3.0(x4096)
  *
  * @param input File
  */

class RawParser[A](input:File) extends Parser {
  // Get iterator over input file, buffer size: 15000
  this.data = Source.fromFile(input, 15000).getLines()
  this.fileName = input.getName

  override def hasNext : Boolean = {
    data.hasNext
  }
  /**
    * @return tuple2(Id, Vector)
    */
  override def next[A]:(Int, Array[A]) = {
    if(this.data.hasNext) {
      // The id can be grabbed on place 49 and forward.
      val id = this.data.next.substring(49).toInt
      val vec = this.data.next.split(" ")
      val tvec = for{
        component <- vec
      } yield {
        this match {
          case _:RawParser[Double] => component.toDouble
          case _:RawParser[Float] => component.toFloat
          case _:RawParser[Boolean] => component.toBoolean
        }
      }

      (id, tvec.asInstanceOf[Array[A]])
    } else {
      throw new Exception("Called next on empty Parser")
    }
  }
  this.size = Source.fromFile(input).getLines().length / 2
}

/**
  * Parses file of format:
  *
  *
  * @param input File
  */
class ReducedParser(input:File) extends Parser {

  override def next: (Int, Array[Double]) = ???

  this.size = Source.fromFile(input).getLines().length

  override def hasNext: Boolean = ???
}

/**
  * Parses file of format:
  *
  * Dataset Queriesset Knnstructure K Measure NumericType
  *
  * @param input File
  */
class TestCasesParser(input:File) {

  def hasNext : Boolean = {
    data.hasNext
  }
  private val data = Source.fromFile(input).getLines()

  def size:Int = Source.fromFile(input).getLines().length
  def next = {

    if(this.data.hasNext) {
      val config = this.data.next.split(" ")
      val measure = config(4)

      config(5) match {
        case "Double" => {
          new TestCase(
            config(4) match {
              case "Cosine" => { Cosine }
              case "Euclidean" => { Euclidean }
            },
            "Double",
            new RawParser(new File(config(0))),
            new RawParser(new File(config(1))),
            config(3).toInt
          )
        }
      }
    } else {
      throw new Exception("Called next on empty Parser")
    }
  }
}

















