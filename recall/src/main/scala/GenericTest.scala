import scala.reflect._
import scala.reflect.runtime.universe._
/**
  * Created by remeeh on 18-02-2017.
  */
object GenericTest extends App {
  abstract class SemiGroup[A] {
    def add(x: A, y:A):A
  }

  abstract class Monoid[A] extends SemiGroup[A] {
    def unit: A
  }

  implicit object doubleMonoid extends Monoid[Double] {
    override def unit: Double = 0.0
    override def add(x: Double, y: Double): Double = x + y }

  implicit object floatMonoid extends Monoid[Float] {
    override def unit = 0f
    override def add(x: Float, y: Float) = x + y
  }

  implicit object booleanMonoid extends Monoid[Boolean] {
    override def unit: Boolean = ???

    override def add(x: Boolean, y: Boolean): Boolean = ???
  }

  def sum[A](xs: Seq[A])(implicit m:Monoid[A]) : A = {
    if(xs.isEmpty) m.unit
    else m.add(xs.head, xs.head)
  }

  def cast[A](a:Any, tt:TypeTag[A]): A = a.asInstanceOf[A]

  println("HELLO")
  val t = "Double"
  // get t to be of class....
  val tc = Class.forName("scala."+t)
  val svec = Array("1.2", "2.0", "1.0")
  val vec = for{
    v <- svec
  } yield v

  val z: Any = "2.0"
  val re = cast(z, typeTag[Double])

  // Define class... call create arr

  val a = createArr(1f,2f,3f)
  val b = createArr(3.0,2.2,5.0)
  val c = createArr("a", "b", "c")

  println(sum(a))
  println(sum(b))
  //println(sum(c))



  def createArr[A : ClassTag](seq: A*) = {
    Array[A](seq: _*)
  }
//  def compute[A](xs: Array[A], ys: Array[A]) = {
//    measure(xs, ys)
//  }
//
//  def cosineDouble(xs:Array[Double], ys:Array[Double]) : Double = {
//    require(xs.length == ys.length)
//    var res = 0.0
//    for(i <- xs.indices) {
//      res+=xs(i) + ys(i)
//    }
//    res
//  }
//
//  def cosineFloat(xs:Array[Float], ys:Array[Float]) : Float = {
//    require(xs.length == ys.length)
//    var res = 0f
//    for(i <- xs.indices) {
//      res += xs(i) + ys(i)
//    }
//    res
//  }

}

