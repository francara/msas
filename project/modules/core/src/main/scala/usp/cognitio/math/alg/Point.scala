package usp.cognitio.math.alg

import scala.Math.pow
import scala.Math.sqrt

class Point(val x : Int, val y : Int) {
  def this(xy: (Int,Int)) = this(xy._1, xy._2)
  def xy : (Int,Int) = (x,y)
  
  def dist(p : (Int,Int)) : Double = dist(Point(p._1,p._2)) 

  def dist(other:Point) : Double = {
    var d = sqrt(pow(x-other.x,2) + pow(y-other.y,2))
    return BigDecimal(d).setScale(2, BigDecimal.RoundingMode.FLOOR).toDouble
  }
  
  def neighs: List[Point] = {
    var cells: List[Point] = List()
    if (x - 1 >= 0) cells = (x - 1, y) :: cells
    if (y - 1 >= 0) cells = (x, y - 1) :: cells
    if (x + 1 < Point.MAX) cells = (x + 1, y) :: cells
    if (y + 1 < Point.MAX) cells = (x, y + 1) :: cells
    return cells
  }
  
  override def toString : String =  "(" + x + "," + y + ")"
  override def equals(o:Any) : Boolean = {
    if (!o.isInstanceOf[Point]) false
    val other = o.asInstanceOf[Point]
    this.x == other.x && this.y == other.y
  }
  override def hashCode : Int = 17 * x + 31 * y
}

object Point {
  var MAX: Int = 10
  def apply(x:Int, y:Int) = new Point(x,y)
  def apply(p : (Int,Int)) = new Point(p._1,p._2)
  
  implicit def XYToPoint(coord: (Int,Int)) : Point = new Point(coord._1, coord._2)
  implicit def PointToXY(point: Point) : (Int,Int) = (point.x, point.y)
  implicit def XYsToPoint(points: List[Point]) : List[(Int,Int)] = points.map(_.xy)
}