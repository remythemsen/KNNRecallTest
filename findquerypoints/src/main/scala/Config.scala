import java.io._

case class Config(
                   data:File = new File("."),
                   n:Int = 0,
                   queries:File = new File("."),
                   outFile:String = "."
                 )


