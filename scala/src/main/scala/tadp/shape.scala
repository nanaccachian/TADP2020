package tadp
import scalafx.scene.paint.Color

import tadp.internal.TADPDrawingAdapter

trait Shape {
  def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter
}

case class rectangle(pointA: Point, pointB: Point) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.rectangle(pointA.toTuple(), pointB.toTuple())
}

case class triangle(pointA: Point, pointB: Point, pointC: Point) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.triangle(pointA.toTuple(), pointB.toTuple(), pointC.toTuple())
}

case class circle(center: Point, radius: Double) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = adapter.circle(center.toTuple(), radius)
}

case class group(shapes: List[Shape]) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shapes.foldLeft(adapter) ((adapter, shape) => shape.draw(adapter))
}

case class color(red: Int, green: Int, blue: Int, shape:Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginColor(Color.rgb(red, green, blue))).end()
}

case class scale(scaleX: Double, scaleY: Double, shape:Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginScale(scaleX, scaleY)).end()
}

case class rotate(grades: Double, shape: Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginRotate(grades)).end()
}

case class translate(transX: Double, transY: Double, shape: Shape) extends Shape {
  override def draw(adapter: TADPDrawingAdapter): TADPDrawingAdapter = shape.draw(adapter.beginTranslate(transX, transY)).end()
}
