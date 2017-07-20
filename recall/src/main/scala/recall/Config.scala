package recall

import java.io._
case class Config(
                   data:File = new File("."),
                   n:Int = 0,
                   k:Int = 50,
                   queryPoints:File = new File("."),
                   knnstructure:File = new File("."),
                   measure:String = ".",
                   testCases:File = new File("."),
                   dataFormat:String = "raw",
                   outDir:String = "."
                 )


