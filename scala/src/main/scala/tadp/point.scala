package tadp

case class Point(x: Double, y: Double) {
  def toTuple(): (Double, Double) = (x, y)
}

object Point {
  implicit class PointBuilder(val x: Double) extends AnyVal {
    def !(y: Double): Point = Point(x, y)
  }
}
