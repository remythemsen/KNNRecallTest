import java.io._

case class Config(
                   data:File = new File("."),
                   n:Int = 0,
                   skipSize:Int = 0,
                     seed:Long = 0,
                   outFile:String = "."
                 )


